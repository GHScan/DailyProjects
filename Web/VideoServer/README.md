# OpenVideo Streaming Server

A simple video-on-demand streaming server that allows users to browse and play video files from a specified directory via a web browser (desktop or mobile).

## Features

- **Directory Browsing**: Recursively lists all video files in a specified directory, displayed in a tree structure.
- **Smooth Playback**: Implements HTTP Range requests, supporting progress bar seeking.
- **Mobile Friendly**: Supports mobile browser access with auto-play and full-screen capabilities.
- **Codec Support**: Supports common video formats like H.264 and H.265 (HEVC) (HEVC support depends on browser hardware acceleration).

## Quick Start

### 1. Install Dependencies

Ensure you have Node.js installed, then run:

```bash
npm install
```

### 2. Start Server

Specify a directory containing videos (e.g., `D:\Videos`) to start the server:

```bash
node server.js --dir "D:\Videos"
```

You can also specify a port:

```bash
node server.js --dir "D:\Videos" --port 3001
```

### 3. Access

Access from any device browser within the same local network:

- Desktop: `http://localhost:3000`
- Mobile/Tablet: `http://[your-server-ip]:3000`

## Important Notes

- **H.265 (HEVC) Playback**: Some browsers (like Chrome) on Windows may require the "HEVC Video Extensions" installed or hardware acceleration enabled to play H.265 videos. Safari on Apple devices has excellent native support.
- **Network**: If you cannot access the server from your phone, ensure your computer's firewall allows traffic on the specified port and both devices are connected to the same router.
