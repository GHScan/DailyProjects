const os = require('os');
const express = require('express');
const path = require('path');
const { createRouter } = require('./server/routes');

const app = express();

// Root directory for comic files
const rootDir = process.argv[2] ? path.resolve(process.argv[2]) : process.cwd();

// Port configuration: Command line arg > Env var > 3000
const PORT = process.argv[3] || process.env.PORT || 3000;

function getLocalIP() {
  const interfaces = os.networkInterfaces();
  for (const name of Object.keys(interfaces)) {
    for (const iface of interfaces[name]) {
      if (iface.family === 'IPv4' && !iface.internal) {
        return iface.address;
      }
    }
  }
  return 'localhost';
}

console.log(`Starting Comic Server...`);
console.log(`Scanning Root Directory: ${rootDir}`);

// Middleware
app.use(express.static(path.join(__dirname, 'public')));

// API routes
app.use('/api', createRouter(rootDir));

// Serve SPA: index.html for all other routes or just for root
app.get('/', (req, res) => {
  res.sendFile(path.join(__dirname, 'public', 'index.html'));
});

// Bind to 0.0.0.0 to allow LAN access
app.listen(PORT, '0.0.0.0', () => {
  const localIP = getLocalIP();
  console.log(`\nServer is running!`);
  console.log(`- Local access:   http://localhost:${PORT}`);
  console.log(`- Network access: http://${localIP}:${PORT}`);
  console.log(`\nUsage: node index.js [root_directory] [port]`);
});
