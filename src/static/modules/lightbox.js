// Lightbox overlay for inline image previews

let overlay = null;
let imgEl = null;
let openLinkEl = null;
let counterEl = null;
let prevBtn = null;
let nextBtn = null;

let gallery = [];
let currentIndex = -1;
let previouslyFocused = null;
let initialized = false;

function getVisibleChatImages() {
    const chat = document.getElementById('chat');
    if (!chat) return [];
    return Array.from(chat.querySelectorAll('img.image-preview')).filter(
        (img) => img.style.display !== 'none' && img.getAttribute('src')
    );
}

function ensureOverlay() {
    if (overlay) return overlay;

    overlay = document.createElement('div');
    overlay.id = 'image-lightbox';
    overlay.className = 'lightbox-overlay';
    overlay.setAttribute('role', 'dialog');
    overlay.setAttribute('aria-modal', 'true');
    overlay.setAttribute('aria-label', 'Image viewer');
    overlay.hidden = true;

    overlay.innerHTML = `
        <div class="lightbox-toolbar">
            <span class="lightbox-counter" aria-live="polite"></span>
            <div class="lightbox-actions">
                <a class="lightbox-btn lightbox-open-link" target="_blank" rel="noopener noreferrer">Open original ↗</a>
                <button type="button" class="lightbox-btn lightbox-close-btn" aria-label="Close image viewer">✕</button>
            </div>
        </div>
        <button type="button" class="lightbox-nav lightbox-prev" aria-label="Previous image">‹</button>
        <div class="lightbox-stage">
            <img class="lightbox-img" alt="Expanded image preview">
        </div>
        <button type="button" class="lightbox-nav lightbox-next" aria-label="Next image">›</button>
    `;

    imgEl = overlay.querySelector('.lightbox-img');
    openLinkEl = overlay.querySelector('.lightbox-open-link');
    counterEl = overlay.querySelector('.lightbox-counter');
    prevBtn = overlay.querySelector('.lightbox-prev');
    nextBtn = overlay.querySelector('.lightbox-next');
    const closeBtn = overlay.querySelector('.lightbox-close-btn');
    const stage = overlay.querySelector('.lightbox-stage');

    closeBtn.addEventListener('click', close);
    prevBtn.addEventListener('click', (e) => {
        e.stopPropagation();
        step(-1);
    });
    nextBtn.addEventListener('click', (e) => {
        e.stopPropagation();
        step(1);
    });

    imgEl.addEventListener('click', (e) => {
        e.stopPropagation();
        imgEl.classList.toggle('zoomed');
    });

    overlay.addEventListener('click', (e) => {
        if (e.target === overlay || e.target === stage) {
            close();
        }
    });

    document.body.appendChild(overlay);
    return overlay;
}

function renderCurrent() {
    if (!overlay || currentIndex < 0 || currentIndex >= gallery.length) return;
    const item = gallery[currentIndex];
    imgEl.classList.remove('zoomed');
    imgEl.src = item.src;
    imgEl.alt = item.alt || 'Expanded image preview';
    openLinkEl.href = item.src;

    const hasMultiple = gallery.length > 1;
    prevBtn.hidden = !hasMultiple;
    nextBtn.hidden = !hasMultiple;
    counterEl.textContent = hasMultiple ? `${currentIndex + 1} / ${gallery.length}` : '';
}

function open(src, alt = '') {
    if (!src) return;
    ensureOverlay();

    const visibleImgs = getVisibleChatImages();
    if (visibleImgs.length > 0) {
        gallery = visibleImgs.map((el) => ({
            src: el.currentSrc || el.src,
            alt: el.alt || ''
        }));
        currentIndex = gallery.findIndex((item) => item.src === src);
        if (currentIndex === -1) {
            gallery.push({ src, alt });
            currentIndex = gallery.length - 1;
        }
    } else {
        gallery = [{ src, alt }];
        currentIndex = 0;
    }

    previouslyFocused = document.activeElement;
    renderCurrent();
    overlay.hidden = false;
    document.body.classList.add('lightbox-open');
    const closeBtn = overlay.querySelector('.lightbox-close-btn');
    if (closeBtn) closeBtn.focus();
}

function step(delta) {
    if (!isOpen() || gallery.length <= 1) return;
    currentIndex = (currentIndex + delta + gallery.length) % gallery.length;
    renderCurrent();
}

function close() {
    if (!overlay || overlay.hidden) return;
    overlay.hidden = true;
    imgEl.classList.remove('zoomed');
    imgEl.removeAttribute('src');
    document.body.classList.remove('lightbox-open');
    gallery = [];
    currentIndex = -1;
    if (previouslyFocused && typeof previouslyFocused.focus === 'function') {
        previouslyFocused.focus();
    }
    previouslyFocused = null;
}

function isOpen() {
    return Boolean(overlay && !overlay.hidden);
}

function handleDocumentClick(e) {
    const target = e.target;
    if (target && target.tagName === 'IMG' && target.classList.contains('image-preview')) {
        e.preventDefault();
        open(target.currentSrc || target.src, target.alt || '');
    }
}

function handleKeyDown(e) {
    if (!isOpen()) return;
    if (e.key === 'Escape') {
        e.preventDefault();
        close();
    } else if (e.key === 'ArrowLeft') {
        e.preventDefault();
        step(-1);
    } else if (e.key === 'ArrowRight') {
        e.preventDefault();
        step(1);
    }
}

function init() {
    if (initialized) return;
    initialized = true;
    document.addEventListener('click', handleDocumentClick);
    document.addEventListener('keydown', handleKeyDown);
}

export default {
    init,
    open,
    close,
    step,
    isOpen
};
