const express = require('express');
const cors = require('cors');
const { createProxyMiddleware } = require('http-proxy-middleware');
const { monitoringMiddleware, getMetrics } = require('./monitoring');

const app = express();
const port = 4000;

// Middleware
app.use(cors());
app.use(express.json());

// Proxy configurations with monitoring
const cicsProxy = createProxyMiddleware('/catalog', {
    target: 'http://localhost:3000',
    pathRewrite: {'^/catalog': ''},
    changeOrigin: true,
    onProxyReq: (proxyReq, req, res) => { // Example of request transformation (like COBOL COMMAREA)
        if (req.method === 'POST' && req.path.includes('/orders')) {
            console.log('Transforming order request to COMMAREA format');
        }
    },
    onProxyRes: (proxyRes, req, res) => {
       
        console.log(`CICS Response: ${proxyRes.statusCode}`);
    }
});

const db2Proxy = createProxyMiddleware('/employees', {
    target: 'http://localhost:3001',
    changeOrigin: true,
    onProxyReq: (proxyReq, req, res) => {
      
        if (req.method === 'GET') {
            console.log('Transforming REST request to SQL query format');
        }
    }
});

const imsProxy = createProxyMiddleware('/phonebook', {
    target: 'http://localhost:3002',
    changeOrigin: true,
    onProxyReq: (proxyReq, req, res) => {
        
        if (req.method === 'POST') {
            console.log('Transforming REST request to IMS segment format');
        }
    }
});

// Monitoring middleware 
app.use('/catalog', monitoringMiddleware('cics'));
app.use('/employees', monitoringMiddleware('db2'));
app.use('/phonebook', monitoringMiddleware('ims'));

// Routing proxies
app.use('/catalog', cicsProxy);
app.use('/employees', db2Proxy);
app.use('/phonebook', imsProxy);

// we can call this endpoint to get the metrics
app.get('/metrics', (req, res) => {
    res.json(getMetrics());
});

// API documentation endpoint, we can call this endpoint to get the API documentation
app.get('/', (req, res) => {
    res.json({
        message: "Z/OS Modernization Mock API Gateway",
        documentation: "Access /metrics for API usage statistics",
        services: {
            "CICS Catalog API": {
                "base_path": "/catalog",
                "endpoints": [
                    "GET /items",
                    "GET /items/{id}",
                    "POST /orders"
                ],
                "transformation": "COMMAREA ↔ JSON"
            },
            "DB2 Employee API": {
                "base_path": "/employees",
                "endpoints": [
                    "GET /employees",
                    "GET /employees/{id}",
                    "POST /employees",
                    "PUT /employees/{id}",
                    "DELETE /employees/{id}"
                ],
                "transformation": "SQL ↔ JSON"
            },
            "IMS Phonebook API": {
                "base_path": "/phonebook",
                "endpoints": [
                    "POST /contacts",
                    "GET /contacts/{lastName}",
                    "PUT /contacts/{lastName}",
                    "DELETE /contacts/{lastName}"
                ],
                "transformation": "IMS Segments ↔ JSON"
            }
        }
    });
});

app.listen(port, () => {
    console.log(`API Gateway running at http://localhost:${port}`);
    console.log('Available endpoints:');
    console.log('- API Documentation: http://localhost:${port}/');
    console.log('- Metrics: http://localhost:${port}/metrics');
    console.log('\nProxying to:');
    console.log('- CICS API: http://localhost:3000');
    console.log('- DB2 API:  http://localhost:3001');
    console.log('- IMS API:  http://localhost:3002');
});