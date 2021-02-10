export async function getAllItems() {
    const response = await fetch('/api/items');
    return await response.json();
}

export async function createOrder(data, table) {
    const slugs = data.map(item => (item.slug))
    const response = await fetch(`/api/orders`, {
        method: 'POST',
        headers: {'Content-Type': 'application/json'},
        body: JSON.stringify({tableNumber: table, items: slugs})
    });
    if (response.status !== 200) {
        return null;
    }
    return response.json();
}

export async function getAllCategories() {
    const response = await fetch('/api/categories');
    return await response.json();
}