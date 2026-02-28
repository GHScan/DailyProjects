const express = require('express');
const { program } = require('commander');
const path = require('path');
const fs = require('fs-extra');
const mime = require('mime-types');

program
  .option('-d, --dir <directory>', 'Directory to serve videos from', process.cwd())
  .option('-p, --port <port>', 'Port to listen on', '3000')
  .parse(process.argv);

const options = program.opts();
const rootDir = path.resolve(options.dir);

if (!fs.existsSync(rootDir)) {
  console.error(`Error: Directory "${rootDir}" does not exist.`);
  process.exit(1);
}

const app = express();

// Serve static files from public directory
app.use(express.static(path.join(__dirname, 'public')));

// Video file extensions to search for
const videoExtensions = ['.mp4', '.mkv', '.mov', '.avi', '.webm', '.m4v'];

/**
 * Recursively get video files in a directory tree
 */
async function getFileTree(dir, baseDir) {
  const result = [];
  const files = await fs.readdir(dir);

  for (const file of files) {
    const fullPath = path.join(dir, file);
    const stats = await fs.stat(fullPath);
    const relativePath = path.relative(baseDir, fullPath);

    if (stats.isDirectory()) {
      const children = await getFileTree(fullPath, baseDir);
      if (children.length > 0) {
        result.push({
          name: file,
          type: 'directory',
          path: relativePath,
          children: children
        });
      }
    } else {
      const ext = path.extname(file).toLowerCase();
      if (videoExtensions.includes(ext)) {
        result.push({
          name: file,
          type: 'file',
          path: relativePath,
          size: stats.size
        });
      }
    }
  }
  
  // Sort: directories first, then files alphabetically
  return result.sort((a, b) => {
    if (a.type === b.type) return a.name.localeCompare(b.name);
    return a.type === 'directory' ? -1 : 1;
  });
}

app.get('/api/files', async (req, res) => {
  try {
    const tree = await getFileTree(rootDir, rootDir);
    res.json(tree);
  } catch (err) {
    console.error('Error reading files:', err);
    res.status(500).json({ error: 'Failed to read video directory' });
  }
});

// Implementation of video streaming API
app.get('/video', async (req, res) => {
  const fileRelativePath = req.query.path;
  if (!fileRelativePath) {
    return res.status(400).send('Path parameter is required');
  }

  const filePath = path.join(rootDir, fileRelativePath);
  
  // Basic security check: ensure requested file is within the root directory
  if (!filePath.startsWith(rootDir)) {
    return res.status(403).send('Forbidden: Access outside base directory');
  }

  try {
    const stat = await fs.stat(filePath);
    const fileSize = stat.size;
    const range = req.headers.range;
    const contentType = mime.lookup(filePath) || 'video/mp4';

    if (range) {
      const parts = range.replace(/bytes=/, "").split("-");
      const start = parseInt(parts[0], 10);
      const end = parts[1] ? parseInt(parts[1], 10) : fileSize - 1;

      if (start >= fileSize) {
        res.status(416).send('Requested range not satisfiable\n' + start + ' >= ' + fileSize);
        return;
      }

      const chunksize = (end - start) + 1;
      const file = fs.createReadStream(filePath, { start, end });
      const head = {
        'Content-Range': `bytes ${start}-${end}/${fileSize}`,
        'Accept-Ranges': 'bytes',
        'Content-Length': chunksize,
        'Content-Type': contentType,
      };

      res.writeHead(206, head);
      file.pipe(res);
    } else {
      const head = {
        'Content-Length': fileSize,
        'Content-Type': contentType,
      };
      res.writeHead(200, head);
      fs.createReadStream(filePath).pipe(res);
    }
  } catch (err) {
    console.error('Error reading video file:', err);
    res.status(404).send('Video file not found');
  }
});

app.listen(options.port, () => {
  console.log(`Video streaming server listening at http://localhost:${options.port}`);
  console.log(`Serving videos from: ${rootDir}`);
});
