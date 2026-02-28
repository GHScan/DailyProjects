const fs = require('fs').promises;
const path = require('path');

const IMAGE_EXTENSIONS = new Set(['.jpg', '.jpeg', '.png', '.gif', '.webp', '.bmp']);

function isImage(filename) {
  return IMAGE_EXTENSIONS.has(path.extname(filename).toLowerCase());
}

async function scanDirectory(dirPath, rootPath) {
  try {
    const entries = await fs.readdir(dirPath, { withFileTypes: true });
    
    let subDirs = [];
    let images = [];

    for (const entry of entries) {
      if (entry.isDirectory()) {
        subDirs.push(entry.name);
      } else if (entry.isFile() && isImage(entry.name)) {
        images.push(entry.name);
      }
    }

    // Natural sort
    subDirs.sort((a, b) => a.localeCompare(b, undefined, { numeric: true, sensitivity: 'base' }));
    images.sort((a, b) => a.localeCompare(b, undefined, { numeric: true, sensitivity: 'base' }));

    const isContentDir = images.length > 1;
    const relativePath = path.relative(rootPath, dirPath);

    const result = {
      path: relativePath,
      name: path.basename(dirPath) || 'Root',
      isContentDir,
      images: isContentDir ? images : [],
      subDirs: []
    };

    if (!isContentDir) {
      for (const subDir of subDirs) {
        const fullSubPath = path.join(dirPath, subDir);
        // Lazy metadata check for subdirs: find first image
        const previewImage = await findFirstImage(fullSubPath);
        result.subDirs.push({
          name: subDir,
          path: path.join(relativePath, subDir),
          hasPreview: !!previewImage,
          previewUrl: previewImage ? `/api/image-preview?path=${encodeURIComponent(path.join(relativePath, subDir, previewImage))}` : null,
          isContentDir: !!previewImage // Hint: if has images, likely content dir
        });
      }
    }

    return result;
  } catch (err) {
    console.error(`Error scanning directory ${dirPath}:`, err);
    throw err;
  }
}

async function findFirstImage(dirPath, currentDepth = 0) {
  const MAX_DEPTH = 5; // Limit recursion depth to avoid excessive traversal
  if (currentDepth > MAX_DEPTH) return null;

  try {
    const entries = await fs.readdir(dirPath, { withFileTypes: true });
    
    // 1. Check files in current directory first
    const files = entries
      .filter(e => e.isFile() && isImage(e.name))
      .map(e => e.name)
      .sort((a, b) => a.localeCompare(b, undefined, { numeric: true, sensitivity: 'base' }));
    
    if (files.length > 0) return files[0];
    
    // 2. If no images, check subdirectories (recursively)
    const subDirs = entries
      .filter(e => e.isDirectory())
      .map(e => e.name)
      .sort((a, b) => a.localeCompare(b, undefined, { numeric: true, sensitivity: 'base' }));

    for (const subDir of subDirs) {
      const fullSubPath = path.join(dirPath, subDir);
      const imageInSubDir = await findFirstImage(fullSubPath, currentDepth + 1);
      if (imageInSubDir) {
        // Return path relative to current dirPath
        return path.join(subDir, imageInSubDir);
      }
    }
    
    return null;
  } catch (err) {
    return null;
  }
}

async function getSiblingDirs(dirPath, rootPath) {
  const parentPath = path.dirname(dirPath);
  const currentName = path.basename(dirPath);
  
  try {
    const entries = await fs.readdir(parentPath, { withFileTypes: true });
    const subDirs = entries
      .filter(e => e.isDirectory())
      .map(e => e.name)
      .sort((a, b) => a.localeCompare(b, undefined, { numeric: true, sensitivity: 'base' }));
    
    const currentIndex = subDirs.indexOf(currentName);
    const prev = currentIndex > 0 ? path.join(path.relative(rootPath, parentPath), subDirs[currentIndex - 1]) : null;
    const next = currentIndex < subDirs.length - 1 ? path.join(path.relative(rootPath, parentPath), subDirs[currentIndex + 1]) : null;
    
    return { prev, next };
  } catch (err) {
    return { prev: null, next: null };
  }
}

module.exports = {
  scanDirectory,
  getSiblingDirs,
  isImage
};
