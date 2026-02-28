const express = require('express');
const path = require('path');
const fs = require('fs');
const { scanDirectory, getSiblingDirs, isImage } = require('./scanner');

function createRouter(rootDir) {
  const router = express.Router();

  const getFullPath = (relPath) => {
    // Basic path sanitation
    const normalized = path.normalize(relPath).replace(/^(\.\.(\/|\\|$))+/, '');
    return path.join(rootDir, normalized);
  };

  router.get('/list', async (req, res) => {
    const relPath = req.query.path || '';
    const fullPath = getFullPath(relPath);

    try {
      const result = await scanDirectory(fullPath, rootDir);
      
      // If it's a content directory, also get siblings for navigation
      if (result.isContentDir) {
        const siblings = await getSiblingDirs(fullPath, rootDir);
        result.siblings = siblings;
      }
      
      res.json(result);
    } catch (err) {
      res.status(500).json({ error: 'Failed to scan directory' });
    }
  });

  // Serve image for previews or direct viewing
  const serveImage = (req, res) => {
    const relPath = req.query.path || '';
    const fullPath = getFullPath(relPath);

    if (!isImage(fullPath)) {
      return res.status(400).send('Not an image');
    }

    if (!fs.existsSync(fullPath)) {
      return res.status(404).send('Image not found');
    }

    res.sendFile(fullPath);
  };

  router.get('/image-preview', serveImage);
  router.get('/image-serve', serveImage);

  return router;
}

module.exports = { createRouter };
