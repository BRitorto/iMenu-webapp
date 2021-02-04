export async function getAllItems() {
    const response = await fetch('/api/items');
    return await response.json();
}

export async function createItem(data) {
    const response = await fetch(`/admin/items`, {
        method: 'POST',
        headers: {'Content-Type': 'application/json'},
        body: JSON.stringify({item: data})
    })
    return await response.json();
}