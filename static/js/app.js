let cart = [];
let products = [];
let currentItem = null;
let currentUser = null;

// Initialize
document.addEventListener('DOMContentLoaded', () => {
    checkLoginStatus();
    fetchProducts();
});

// Authentication Functions
async function checkLoginStatus() {
    try {
        const response = await fetch('/api/status');
        const data = await response.json();

        const guestArea = document.getElementById('guest-area');
        const userArea = document.getElementById('user-area');
        const navUsername = document.getElementById('nav-username');
        const guestInfoForm = document.getElementById('guest-info-form');
        const userInfoDisplay = document.getElementById('user-info-display');
        const displayUser = document.getElementById('display-user');

        if (data.logged_in) {
            currentUser = data;
            guestArea?.classList.add('hidden');
            userArea?.classList.remove('hidden');
            if (navUsername) navUsername.innerText = data.username;

            guestInfoForm?.classList.add('hidden');
            userInfoDisplay?.classList.remove('hidden');
            if (displayUser) displayUser.innerText = data.username;
        } else {
            currentUser = null;
            guestArea?.classList.remove('hidden');
            userArea?.classList.add('hidden');

            guestInfoForm?.classList.remove('hidden');
            userInfoDisplay?.classList.add('hidden');
        }
    } catch (error) {
        console.error('Failed to check status:', error);
    }
}

async function sendOtp() {
    const phone = document.getElementById('reg-phone').value;
    if (!phone) return alert('請先輸入手機號碼');

    const btn = document.getElementById('btn-otp');
    btn.disabled = true;
    btn.innerText = '發送中...';

    try {
        const response = await fetch('/api/send_otp', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ phone })
        });
        const res = await response.json();
        if (res.status === 'success') {
            alert('驗證碼已發送！請查看背景終端機');
            btn.innerText = '已發送';
        } else {
            alert(res.msg);
            btn.disabled = false;
            btn.innerText = '取得驗證碼';
        }
    } catch (error) {
        alert('發送失敗');
        btn.disabled = false;
        btn.innerText = '取得驗證碼';
    }
}

async function register() {
    const username = document.getElementById('reg-name').value;
    const phone = document.getElementById('reg-phone').value;
    const password = document.getElementById('reg-pwd').value;
    const otp = document.getElementById('reg-otp').value;

    if (!username || !phone || !password || !otp) return alert('請填寫所有欄位');

    try {
        const response = await fetch('/api/register', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ username, phone, password, otp })
        });
        const res = await response.json();
        if (res.status === 'success') {
            alert('註冊成功！請登入');
            closeModal('register-modal');
            openModal('login-modal');
        } else {
            alert(res.msg);
        }
    } catch (error) {
        alert('註冊發生錯誤');
    }
}

async function login() {
    const phone = document.getElementById('login-phone').value;
    const password = document.getElementById('login-pwd').value;

    if (!phone || !password) return alert('請填寫手機與密碼');

    try {
        const response = await fetch('/api/login', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ phone, password })
        });
        const res = await response.json();
        if (res.status === 'success') {
            location.reload();
        } else {
            alert(res.msg);
        }
    } catch (error) {
        alert('登入發生錯誤');
    }
}

async function logout() {
    await fetch('/api/logout');
    location.reload();
}

async function fetchHistory() {
    try {
        const response = await fetch('/api/my_history');
        const list = await response.json();
        const historyList = document.getElementById('history-list');

        if (list.status === 'error') {
            alert(list.msg);
            return;
        }

        historyList.innerHTML = list.length
            ? list.map(i => `
                <div class="bg-white p-4 rounded-xl shadow-sm border border-gray-100">
                    <div class="flex justify-between items-start mb-2">
                        <span class="text-xs text-gray-400">${i.time}</span>
                        <span class="px-2 py-0.5 rounded-full text-xs font-medium ${i.status === 'pending' ? 'bg-yellow-100 text-yellow-700' : 'bg-green-100 text-green-700'
                }">
                            ${i.status === 'pending' ? '製作中' : '已完成'}
                        </span>
                    </div>
                    <div class="font-medium text-gray-800">${i.item}</div>
                    <div class="text-sm text-gray-500">${i.options} x${i.qty}</div>
                    <div class="text-right font-bold text-rose-600 mt-1">$${i.price}</div>
                </div>
            `).join('')
            : '<div class="text-center text-gray-400 py-8">目前尚無訂單紀錄</div>';

        openModal('history-modal');
    } catch (error) {
        console.error('Failed to fetch history:', error);
    }
}

// Menu & Cart Functions
async function fetchProducts() {
    try {
        const response = await fetch('/api/products');
        products = await response.json();
        renderMenu();
    } catch (error) {
        console.error('Failed to fetch products:', error);
    }
}

function renderMenu() {
    const container = document.getElementById('menu-container');
    container.innerHTML = products.map(item => `
        <div class="drink-card product-card bg-white rounded-2xl shadow-sm overflow-hidden cursor-pointer border border-gray-50" onclick="openOptions(${item.id})">
            <div class="relative h-40 bg-gray-100">
                <img src="/static/${item.image}" alt="${item.name}" class="w-full h-full object-contain p-4">
                <div class="absolute bottom-2 right-2 bg-white/90 backdrop-blur px-2 py-1 rounded-lg shadow-sm">
                    <span class="text-rose-600 font-bold">$${item.price}</span>
                </div>
            </div>
            <div class="p-4">
                <h3 class="font-bold text-gray-800 text-lg mb-1">${item.name}</h3>
                <div class="flex items-center text-gray-400 text-xs">
                    <svg class="w-3 h-3 mr-1" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M11.049 2.927c.3-.921 1.603-.921 1.902 0l1.519 4.674a1 1 0 00.95.69h4.915c.969 0 1.371 1.24.588 1.81l-3.976 2.888a1 1 0 00-.363 1.118l1.518 4.674c.3.921-.755 1.688-1.54 1.118l-3.976-2.888a1 1 0 00-1.175 0l-3.976 2.888c-.784.57-1.838-.197-1.539-1.118l1.518-4.674a1 1 0 00-.363-1.118l-3.976-2.888c-.784-.57-.383-1.81.588-1.81h4.914a1 1 0 00.951-.69l1.519-4.674z"></path></svg>
                    <span>人氣推薦</span>
                </div>
            </div>
        </div>
    `).join('');
}

function openOptions(id) {
    currentItem = products.find(p => p.id === id);
    document.getElementById('modal-title').innerText = currentItem.name;
    document.getElementById('modal-price').innerText = `$${currentItem.price}`;
    document.getElementById('modal-img').src = `/static/${currentItem.image}`;
    document.getElementById('qty').value = 1;
    openModal('option-modal');
}

// Toast Notification System
function showToast(message) {
    const toast = document.createElement('div');
    toast.className = 'fixed bottom-10 left-1/2 transform -translate-x-1/2 bg-black/80 backdrop-blur-md text-white px-6 py-3 rounded-full text-sm font-medium z-[100] shadow-2xl animate-in fade-in slide-in-from-bottom-4 duration-300';
    toast.innerHTML = `
        <div class="flex items-center space-x-2">
            <svg class="w-4 h-4 text-green-400" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 13l4 4L19 7"></path></svg>
            <span>${message}</span>
        </div>
    `;
    document.body.appendChild(toast);
    setTimeout(() => {
        toast.classList.add('fade-out', 'slide-out-to-bottom-4');
        setTimeout(() => toast.remove(), 300);
    }, 2000);
}

function addToCartConfirm() {
    const sugar = document.getElementById('sugar').value;
    const ice = document.getElementById('ice').value;
    const qty = parseInt(document.getElementById('qty').value);

    cart.push({
        ...currentItem,
        sugar,
        ice,
        qty
    });

    updateCartUI();
    closeModal('option-modal');
    showToast(`已加入：${currentItem.name}`);
}

function updateCartUI() {
    const cartCount = document.getElementById('cart-count');
    const cartItems = document.getElementById('cart-items');
    const cartTotal = document.getElementById('cart-total');

    cartCount.innerText = cart.length;
    cartCount.classList.toggle('hidden', cart.length === 0);

    if (cart.length === 0) {
        cartItems.innerHTML = '<div class="text-center py-8 text-gray-400">購物車是空的</div>';
        cartTotal.innerText = '$0';
        return;
    }

    cartItems.innerHTML = cart.map((i, idx) => `
        <div class="flex justify-between items-center bg-gray-50 p-3 rounded-xl mb-2">
            <div>
                <div class="font-medium text-gray-800">${i.name}</div>
                <div class="text-xs text-gray-500">${i.sugar} / ${i.ice} x ${i.qty}</div>
            </div>
            <div class="flex items-center">
                <span class="font-bold text-gray-700 mr-4">$${i.price * i.qty}</span>
                <button onclick="removeFromCart(${idx})" class="text-red-400 hover:text-red-600">
                    <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 7l-.867 12.142A2 2 0 0116.138 21H7.862a2 2 0 01-1.995-1.858L5 7m5 4v6m4-6v6m1-10V4a1 1 0 00-1-1h-4a1 1 0 00-1 1v3M4 7h16"></path></svg>
                </button>
            </div>
        </div>
    `).join('');

    const total = cart.reduce((a, b) => a + (b.price * b.qty), 0);
    cartTotal.innerText = `$${total}`;
}

function removeFromCart(idx) {
    cart.splice(idx, 1);
    updateCartUI();
}

async function checkout() {
    if (cart.length === 0) return alert('購物車是空的');

    let name, phone;
    if (currentUser) {
        name = currentUser.username;
        phone = currentUser.phone;
    } else {
        name = document.getElementById('c-name').value;
        phone = document.getElementById('c-phone').value;
        if (!name || !phone) return alert('請填寫取餐資料');
    }

    try {
        const response = await fetch('/api/checkout', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({
                customer_name: name,
                customer_phone: phone,
                items: cart
            })
        });
        const res = await response.json();
        if (res.status === 'success') {
            showToast('訂單已送出，稍後為您製作！');
            cart = [];
            updateCartUI();
            closeModal('cart-modal');
        } else {
            showToast(res.msg || '結帳失敗');
        }
    } catch (error) {
        alert('連線失敗');
    }
}

// Modal Toggle Utilities
function openModal(id) {
    const modal = document.getElementById(id);
    modal.classList.remove('hidden');
    document.body.style.overflow = 'hidden';
    modal.querySelector('.modal-content')?.classList.add('modal-enter');
}

function closeModal(id) {
    const modal = document.getElementById(id);
    modal.classList.add('hidden');
    document.body.style.overflow = 'auto';
}

// Close modal on backdrop click
window.onclick = function (event) {
    if (event.target.classList.contains('backdrop')) {
        event.target.closest('.modal-container').classList.add('hidden');
        document.body.style.overflow = 'auto';
    }
}

// Filtering logic
const categories = {
    'tea': ['茉香綠茶', '四季春青茶', '金萱茶', '鐵觀音', '格雷伯爵紅茶', '冬瓜玉露', '桂花烏龍', '花鳥那堤'],
    'milk': ['紅/綠鮮奶茶', '格雷伯爵鮮奶', '金萱/觀音鮮奶', '熊貓珍珠鮮奶', '可可/阿華田鮮奶', '黑糖/冬瓜鮮奶', '布丁鮮奶', '仙草凍鮮奶', '芋見泥'],
    'milktea': ['醇香奶茶', '珍珠奶茶', '泰式奶茶', '布丁奶茶', '仙草凍奶茶', '椰果奶茶', '黑糖奶茶', '燕麥仁奶茶'],
    'fruit': ['鮮檸蜜蘆薈', '翡翠檸檬青', '蜂蜜檸檬', '檸檬冬瓜', '百香果綠', '百香多多', '港式凍檸茶', '青茶QQ', '冰淇淋紅茶', '多多綠茶']
};

function filterMenu(category, btnElement) {
    // 1. UI: Update button styles
    document.querySelectorAll('.category-btn').forEach(b => {
        b.classList.remove('active', 'bg-rose-600', 'text-white', 'shadow-md', 'shadow-rose-100');
        b.classList.add('bg-white', 'text-gray-500');
    });

    btnElement.classList.add('active', 'bg-rose-600', 'text-white', 'shadow-md', 'shadow-rose-100');
    btnElement.classList.remove('bg-white', 'text-gray-500');

    // 2. Logic: Precise Allowlist Filtering
    const products = document.querySelectorAll('.product-card');

    products.forEach(card => {
        const name = card.querySelector('h3').innerText.trim();
        let shouldShow = false;

        if (category === 'all') {
            shouldShow = true;
        } else {
            // Only show if the name exists exactly in the category list
            if (categories[category] && categories[category].includes(name)) {
                shouldShow = true;
            }
        }

        if (shouldShow) {
            card.style.display = 'block';
            card.classList.add('animate-in', 'fade-in', 'zoom-in');
        } else {
            card.style.display = 'none';
        }
    });
}
