// GPU-accelerated lightbox overlay for inline image previews (desktop & mobile PWA)

const MIN_SCALE = 1;
const MAX_SCALE = 5;
const DOUBLE_TAP_SCALE = 2.5;
const DOUBLE_TAP_MS = 280;
const DRAG_THRESHOLD_PX = 6;
const SWIPE_THRESHOLD_PX = 60;

let overlay = null;
let stageEl = null;
let imgEl = null;
let openLinkEl = null;
let counterEl = null;
let prevBtn = null;
let nextBtn = null;

let gallery = [];
let currentIndex = -1;
let previouslyFocused = null;
let initialized = false;

// Transform state (always GPU-composited via translate3d + scale)
let scale = 1;
let tx = 0;
let ty = 0;
let rafId = null;

// Pointer / gesture state
const activePointers = new Map();
let panStartX = 0;
let panStartY = 0;
let panOriginTx = 0;
let panOriginTy = 0;
let pointerDownX = 0;
let pointerDownY = 0;
let didMove = false;
let pinchStartDist = 0;
let pinchStartScale = 1;
let pinchStartTx = 0;
let pinchStartTy = 0;
let pinchStartMidX = 0;
let pinchStartMidY = 0;
let lastTapTime = 0;
let lastTapX = 0;
let lastTapY = 0;
let lastPointerType = 'mouse';
let suppressNextClick = false;

function getVisibleChatImages() {
    const chat = document.getElementById('chat');
    if (!chat) return [];
    return Array.from(chat.querySelectorAll('img.image-preview')).filter(
        (img) => img.style.display !== 'none' && img.getAttribute('src')
    );
}

function clamp(val, min, max) {
    return Math.min(max, Math.max(min, val));
}

function clampTranslation(nextScale, nextTx, nextTy) {
    if (!imgEl || !stageEl || nextScale <= 1.001) {
        return { x: 0, y: 0 };
    }
    const baseW = imgEl.offsetWidth || 0;
    const baseH = imgEl.offsetHeight || 0;
    const stageW = stageEl.clientWidth || (typeof window !== 'undefined' ? window.innerWidth : 0);
    const stageH = stageEl.clientHeight || (typeof window !== 'undefined' ? window.innerHeight : 0);

    const maxOffsetX = Math.max(0, (baseW * nextScale - stageW) / 2);
    const maxOffsetY = Math.max(0, (baseH * nextScale - stageH) / 2);

    return {
        x: clamp(nextTx, -maxOffsetX, maxOffsetX),
        y: clamp(nextTy, -maxOffsetY, maxOffsetY)
    };
}

function commitTransform() {
    rafId = null;
    if (!imgEl) return;
    imgEl.style.transform = `translate3d(${tx}px, ${ty}px, 0) scale(${scale})`;
    imgEl.classList.toggle('zoomed', scale > 1.01);
}

function scheduleTransform(animate = false) {
    if (!imgEl) return;
    imgEl.classList.toggle('dragging', !animate);
    if (typeof requestAnimationFrame === 'function') {
        if (rafId === null) {
            rafId = requestAnimationFrame(commitTransform);
        }
    } else {
        commitTransform();
    }
}

function resetTransform(animate = false) {
    scale = 1;
    tx = 0;
    ty = 0;
    scheduleTransform(animate);
}

function zoomToPoint(targetScale, clientX, clientY, animate = true) {
    const nextScale = clamp(targetScale, MIN_SCALE, MAX_SCALE);
    if (nextScale <= 1.01) {
        resetTransform(animate);
        return;
    }

    let nextTx = tx;
    let nextTy = ty;
    if (stageEl && typeof stageEl.getBoundingClientRect === 'function') {
        const rect = stageEl.getBoundingClientRect();
        const cx = rect.left + rect.width / 2;
        const cy = rect.top + rect.height / 2;
        const dx = clientX - cx;
        const dy = clientY - cy;
        const ratio = nextScale / scale;
        nextTx = dx - ratio * (dx - tx);
        nextTy = dy - ratio * (dy - ty);
    }

    const clamped = clampTranslation(nextScale, nextTx, nextTy);
    scale = nextScale;
    tx = clamped.x;
    ty = clamped.y;
    scheduleTransform(animate);
}

function getTwoPointers() {
    const iter = activePointers.values();
    return [iter.next().value, iter.next().value];
}

function pointerDistance(p1, p2) {
    return Math.hypot(p2.x - p1.x, p2.y - p1.y);
}

function startPinch() {
    const [p1, p2] = getTwoPointers();
    pinchStartDist = Math.max(1, pointerDistance(p1, p2));
    pinchStartScale = scale;
    pinchStartTx = tx;
    pinchStartTy = ty;
    pinchStartMidX = (p1.x + p2.x) / 2;
    pinchStartMidY = (p1.y + p2.y) / 2;
}

function handlePointerDown(e) {
    if (!isOpen()) return;
    if (e.pointerType === 'mouse' && e.button !== 0) return;

    lastPointerType = e.pointerType || 'mouse';
    activePointers.set(e.pointerId, { x: e.clientX, y: e.clientY, type: lastPointerType });
    if (stageEl && typeof stageEl.setPointerCapture === 'function') {
        try { stageEl.setPointerCapture(e.pointerId); } catch (err) { /* ignore */ }
    }

    if (activePointers.size === 1) {
        didMove = false;
        suppressNextClick = false;
        pointerDownX = e.clientX;
        pointerDownY = e.clientY;
        panStartX = e.clientX;
        panStartY = e.clientY;
        panOriginTx = tx;
        panOriginTy = ty;
    } else if (activePointers.size === 2) {
        didMove = true;
        startPinch();
    }
}

function handlePointerMove(e) {
    if (!activePointers.has(e.pointerId)) return;
    activePointers.set(e.pointerId, { x: e.clientX, y: e.clientY, type: e.pointerType });

    if (activePointers.size === 2) {
        const [p1, p2] = getTwoPointers();
        const dist = pointerDistance(p1, p2);
        const midX = (p1.x + p2.x) / 2;
        const midY = (p1.y + p2.y) / 2;
        const nextScale = clamp(pinchStartScale * (dist / pinchStartDist), MIN_SCALE, MAX_SCALE);

        let nextTx = pinchStartTx + (midX - pinchStartMidX);
        let nextTy = pinchStartTy + (midY - pinchStartMidY);
        if (stageEl && typeof stageEl.getBoundingClientRect === 'function') {
            const rect = stageEl.getBoundingClientRect();
            const cx = rect.left + rect.width / 2;
            const cy = rect.top + rect.height / 2;
            const dx = pinchStartMidX - cx;
            const dy = pinchStartMidY - cy;
            const ratio = nextScale / pinchStartScale;
            nextTx = (midX - cx) - ratio * (dx - pinchStartTx);
            nextTy = (midY - cy) - ratio * (dy - pinchStartTy);
        }

        const clamped = clampTranslation(nextScale, nextTx, nextTy);
        scale = nextScale;
        tx = clamped.x;
        ty = clamped.y;
        scheduleTransform(false);
        return;
    }

    if (activePointers.size === 1) {
        const moveDist = Math.hypot(e.clientX - pointerDownX, e.clientY - pointerDownY);
        if (moveDist > DRAG_THRESHOLD_PX) {
            didMove = true;
        }
        if (scale > 1.01) {
            const nextTx = panOriginTx + (e.clientX - panStartX);
            const nextTy = panOriginTy + (e.clientY - panStartY);
            const clamped = clampTranslation(scale, nextTx, nextTy);
            tx = clamped.x;
            ty = clamped.y;
            scheduleTransform(false);
        }
    }
}

function handlePointerUp(e) {
    const info = activePointers.get(e.pointerId);
    if (!info) return;
    activePointers.delete(e.pointerId);

    if (activePointers.size === 1) {
        const remaining = activePointers.values().next().value;
        panStartX = remaining.x;
        panStartY = remaining.y;
        panOriginTx = tx;
        panOriginTy = ty;
        return;
    }

    if (activePointers.size === 0) {
        if (didMove) {
            suppressNextClick = true;
            if (scale <= 1.01 && gallery.length > 1) {
                const dx = e.clientX - pointerDownX;
                const dy = e.clientY - pointerDownY;
                if (Math.abs(dx) >= SWIPE_THRESHOLD_PX && Math.abs(dx) > Math.abs(dy) * 1.5) {
                    step(dx < 0 ? 1 : -1);
                }
            }
            if (scale < 1.02) {
                resetTransform(true);
            }
            return;
        }

        // Touch double-tap to zoom in/out cleanly on mobile PWA
        if (info.type === 'touch' && e.target === imgEl) {
            const now = Date.now();
            const tapDist = Math.hypot(e.clientX - lastTapX, e.clientY - lastTapY);
            if (now - lastTapTime <= DOUBLE_TAP_MS && tapDist < 40) {
                lastTapTime = 0;
                suppressNextClick = true;
                if (scale > 1.05) {
                    resetTransform(true);
                } else {
                    zoomToPoint(DOUBLE_TAP_SCALE, e.clientX, e.clientY, true);
                }
            } else {
                lastTapTime = now;
                lastTapX = e.clientX;
                lastTapY = e.clientY;
            }
        }
    }
}

function handleWheel(e) {
    if (!isOpen()) return;
    e.preventDefault();
    const factor = e.deltaY < 0 ? 1.2 : 1 / 1.2;
    zoomToPoint(scale * factor, e.clientX, e.clientY, false);
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
            <img class="lightbox-img" alt="Expanded image preview" draggable="false">
        </div>
        <button type="button" class="lightbox-nav lightbox-next" aria-label="Next image">›</button>
    `;

    stageEl = overlay.querySelector('.lightbox-stage');
    imgEl = overlay.querySelector('.lightbox-img');
    openLinkEl = overlay.querySelector('.lightbox-open-link');
    counterEl = overlay.querySelector('.lightbox-counter');
    prevBtn = overlay.querySelector('.lightbox-prev');
    nextBtn = overlay.querySelector('.lightbox-next');
    const closeBtn = overlay.querySelector('.lightbox-close-btn');

    closeBtn.addEventListener('click', close);
    prevBtn.addEventListener('click', (e) => {
        e.stopPropagation();
        step(-1);
    });
    nextBtn.addEventListener('click', (e) => {
        e.stopPropagation();
        step(1);
    });

    stageEl.addEventListener('pointerdown', handlePointerDown);
    stageEl.addEventListener('pointermove', handlePointerMove);
    stageEl.addEventListener('pointerup', handlePointerUp);
    stageEl.addEventListener('pointercancel', handlePointerUp);
    stageEl.addEventListener('wheel', handleWheel, { passive: false });

    imgEl.addEventListener('click', (e) => {
        e.stopPropagation();
        if (suppressNextClick) {
            suppressNextClick = false;
            return;
        }
        if (lastPointerType === 'touch') return;
        if (scale > 1.05) {
            resetTransform(true);
        } else {
            zoomToPoint(DOUBLE_TAP_SCALE, e.clientX || 0, e.clientY || 0, true);
        }
    });

    overlay.addEventListener('click', (e) => {
        if (suppressNextClick) {
            suppressNextClick = false;
            return;
        }
        if (e.target === overlay || e.target === stageEl) {
            close();
        }
    });

    document.body.appendChild(overlay);
    return overlay;
}

function renderCurrent() {
    if (!overlay || currentIndex < 0 || currentIndex >= gallery.length) return;
    const item = gallery[currentIndex];
    activePointers.clear();
    suppressNextClick = false;
    resetTransform(false);
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
    activePointers.clear();
    if (rafId !== null && typeof cancelAnimationFrame === 'function') {
        cancelAnimationFrame(rafId);
        rafId = null;
    }
    overlay.hidden = true;
    scale = 1;
    tx = 0;
    ty = 0;
    imgEl.classList.remove('zoomed', 'dragging');
    imgEl.style.transform = '';
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
    isOpen,
    zoomToPoint,
    resetTransform
};
