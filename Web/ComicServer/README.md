# Node Comic Server

A high-performance comic server built with Node.js, specifically designed to handle massive comic libraries. It uses **lazy scanning** and **frontend lazy loading** to ensure fast response times and low memory usage, even with thousands of directories or chapters containing hundreds of images.

## Features

*   **Lazy Scanning**: The server does not index the entire library on startup. It only reads directory contents on demand when a user visits a specific path, minimizing startup time and memory overhead.
*   **Smart Directory Identification**:
    *   **Navigation Mode**: Displays subdirectories as a card list if the directory contains other folders. It automatically picks the first image from subdirectories to show as a cover preview.
    *   **Reader Mode**: Automatically identifies a directory as comic content if it directly contains more than one image.
*   **Infinite Scroll & Lazy Loading**: The reader uses the `Intersection Observer API`. Images are only loaded when they are about to enter the viewport, preventing browser crashes from loading too many images at once.
*   **Responsive Width**: Comic images automatically adapt to the browser window width for the best reading experience.
*   **Chapter Navigation**: "Previous" and "Next" buttons are provided at both the top and bottom of the reader to quickly switch between sibling directories (chapters).
*   **Natural Sorting**: Files and directories are sorted naturally (e.g., `1.jpg`, `2.jpg`, `10.jpg`), ensuring the order matches reading expectations.

## Quick Start

### Prerequisites

*   [Node.js](https://nodejs.org/) installed (v14 or higher recommended)

### Installation

1.  Clone or download this project.
2.  Install dependencies in the project root:

```bash
npm install
```

### Running

Start the server by specifying your comic root directory and an optional port:

```bash
node index.js "C:\Path\To\Your\Comics" 8080
```

*   **Root Directory**: Use double quotes if the path contains spaces.
*   **Port**: Defaults to 3000 if not specified.
*   **LAN Access**: The server listens on all network interfaces. Upon startup, it displays your local IP address for easy access from mobile phones or other devices on the same network.

Once started, open your browser and go to: [http://localhost:3000](http://localhost:3000) (or your specified port).

## Project Structure

```text
├── index.js            # Server entry point, configures Express and static assets
├── server/
│   ├── scanner.js      # Core logic: filesystem scanning, type identification, sibling navigation
│   └── routes.js       # API routes: provides directory lists and image streaming
├── public/
│   ├── index.html      # Main page structure
│   ├── style.css       # Responsive styles and reader layout
│   └── app.js          # Frontend logic: rendering, navigation, lazy loading
└── package.json        # Project dependencies and configuration
```

## Tech Stack

*   **Backend**: Node.js, Express
*   **Frontend**: Vanilla JS, CSS Grid/Flexbox, Intersection Observer API
*   **File Handling**: `fs.promises`

## FAQ

**Q: Why are images loading slowly?**
A: The server streams original image files directly. If your images are very large (e.g., >10MB each), loading speed depends on your disk I/O and network bandwidth.

**Q: Why do some directories lack preview images?**
A: The system only checks the **immediate children** of a subdirectory for images. It does not recurse deeply to maintain high performance.
