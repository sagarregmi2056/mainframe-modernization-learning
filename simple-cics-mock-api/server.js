const express = require('express');
const cors = require('cors');

const { catalogItems } = require('./mockData');

const app = express();
const port = 3000;

// Middleware
app.use(cors());
app.use(express.json());

// API Routes
// GET /items - Get all items
app.get('/items', (req, res) => {
    const startItemID = req.query.startItemID || '0';
    const items = catalogItems.filter(item => 
        item.information.itemReference >= parseInt(startItemID)
    );

    res.json({
        totalItems: items.length,
        items: items
    });
});

// GET /items/{id} - Get specific item
app.get('/items/:id', (req, res) => {
    const itemId = parseInt(req.params.id);
    const item = catalogItems.find(item => 
        item.information.itemReference === itemId
    );

    if (!item) {
        return res.status(404).json({
            message: "Item could not be found"
        });
    }

    res.json(item);
});

// POST /orders - Place an order
app.post('/orders', (req, res) => {
    const { itemNumber, quantity } = req.query;
    
    if (!itemNumber || !quantity) {
        return res.status(400).json({
            message: "itemNumber and quantity are required"
        });
    }

    const item = catalogItems.find(item => 
        item.information.itemReference === parseInt(itemNumber)
    );

    if (!item) {
        return res.status(404).json({
            message: "Item not found"
        });
    }

    // Mock successful order
    res.json({
        message: `Order placed successfully for ${quantity} units of item ${itemNumber}`
    });
});

// Basic API documentation endpoint
app.get('/', (req, res) => {
    res.json({
        message: "Catalog Manager API",
        endpoints: {
            "GET /items": "List all items (use startItemID query parameter to paginate)",
            "GET /items/:id": "Get specific item details",
            "POST /orders": "Place an order (requires itemNumber and quantity query parameters)"
        }
    });
});

// Start server
app.listen(port, () => {
    console.log(`Catalog API server running at http://localhost:${port}`);
});
