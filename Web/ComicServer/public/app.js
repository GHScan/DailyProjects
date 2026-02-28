// Main App Logic
const state = {
  currentPath: '',
  isContentDir: false,
  images: [],
  subDirs: [],
  siblings: { prev: null, next: null },
  history: [],
};

const dom = {
  breadcrumb: document.getElementById('breadcrumb'),
  dirList: document.getElementById('dir-list'),
  imageList: document.getElementById('image-list'),
  navView: document.getElementById('nav-view'),
  readerView: document.getElementById('reader-view'),
  btnPrev: document.getElementById('btn-prev'),
  btnNext: document.getElementById('btn-next'),
  btnPrevFooter: document.getElementById('btn-prev-footer'),
  btnNextFooter: document.getElementById('btn-next-footer'),
};

async function navigateTo(path) {
  state.currentPath = path;
  try {
    const res = await fetch(`/api/list?path=${encodeURIComponent(path)}`);
    const data = await res.json();
    
    state.isContentDir = data.isContentDir;
    state.images = data.images || [];
    state.subDirs = data.subDirs || [];
    state.siblings = data.siblings || { prev: null, next: null };
    
    renderBreadcrumb();
    if (state.isContentDir) {
      renderReader();
    } else {
      renderNavigation();
    }
    
    // Smooth scroll to top
    window.scrollTo({ top: 0, behavior: 'smooth' });
    
    // Update navigation buttons
    updateNavButtons();
  } catch (err) {
    console.error('Navigation error:', err);
    alert('加载内容失败');
  }
}

function renderBreadcrumb() {
  const parts = state.currentPath.split(/[/\\]/).filter(Boolean);
  let html = '<a href="#" data-path="">Root</a>';
  let pathAccumulator = '';
  
  parts.forEach((part, index) => {
    pathAccumulator += (index === 0 ? '' : '/') + part;
    html += `<a href="#" data-path="${pathAccumulator}">${part}</a>`;
  });
  
  dom.breadcrumb.innerHTML = html;
  
  // Attach breadcrumb listeners
  dom.breadcrumb.querySelectorAll('a').forEach(a => {
    a.onclick = (e) => {
      e.preventDefault();
      navigateTo(a.dataset.path);
    };
  });
}

function renderNavigation() {
  dom.navView.classList.remove('hidden');
  dom.readerView.classList.add('hidden');
  
  dom.dirList.innerHTML = state.subDirs.map(dir => `
    <li class="dir-card" data-path="${dir.path}">
      <div class="preview-container">
        ${dir.hasPreview 
          ? `<img src="${dir.previewUrl}" alt="${dir.name}" loading="lazy">` 
          : '<div class="folder-icon">📁</div>'}
      </div>
      <div class="dir-name">${dir.name}</div>
    </li>
  `).join('');
  
  // Attach listeners to cards
  dom.dirList.querySelectorAll('.dir-card').forEach(card => {
    card.onclick = () => navigateTo(card.dataset.path);
  });
}

function renderReader() {
  dom.navView.classList.add('hidden');
  dom.readerView.classList.remove('hidden');
  
  dom.imageList.innerHTML = state.images.map((img, index) => {
    const imgPath = `${state.currentPath}/${img}`;
    return `
      <div class="image-wrapper" data-index="${index}">
        <img 
          data-src="/api/image-serve?path=${encodeURIComponent(imgPath)}" 
          alt="${img}"
          class="lazy-image"
        >
      </div>
    `;
  }).join('');
  
  setupLazyLoading();
}

function setupLazyLoading() {
  const options = {
    root: null, // use viewport
    rootMargin: '500px 0px', // start loading before entering
    threshold: 0.01
  };
  
  const observer = new IntersectionObserver((entries, observer) => {
    entries.forEach(entry => {
      if (entry.isIntersecting) {
        const img = entry.target;
        if (img.dataset.src) {
          img.src = img.dataset.src;
          img.removeAttribute('data-src');
          observer.unobserve(img); // Stop observing after loading
        }
      }
    });
  }, options);
  
  document.querySelectorAll('.lazy-image').forEach(img => {
    observer.observe(img);
  });
}

function updateNavButtons() {
  const { prev, next } = state.siblings;
  
  const updateBtn = (btn, path) => {
    if (path !== null) {
      btn.hidden = false;
      btn.onclick = () => navigateTo(path);
    } else {
      btn.hidden = true;
    }
  };
  
  updateBtn(dom.btnPrev, prev);
  updateBtn(dom.btnNext, next);
  updateBtn(dom.btnPrevFooter, prev);
  updateBtn(dom.btnNextFooter, next);
}

// Initial navigation
navigateTo('');

// Handle browser back button (optional, but good for UX)
window.onpopstate = () => {
  // Simple: reload root for now or manage state history properly
};
