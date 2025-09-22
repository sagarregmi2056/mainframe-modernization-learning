# API Gateway for Mainframe Service Simulation

## Overview
This API Gateway serves as the central entry point for simulated mainframe services (CICS, DB2, and IMS). It demonstrates how z/OS Connect Enterprise Edition works in a real mainframe environment.

## Architecture
```
Client Requests → API Gateway
     ↓              ↓
JSON/REST    Data Transformation
     ↓              ↓
     └→ CICS Service (Catalog)
     └→ DB2 Service  (Employees)
     └→ IMS Service  (Phonebook)
```

## Features
- Request routing to appropriate backend services
- Data transformation simulation
- Request/Response monitoring
- Metrics collection
- Service documentation

## Service Mappings

### CICS Catalog Service
```
Endpoint: /catalog/*
Target: http://localhost:3000
Transformations: REST ↔ COMMAREA
```

### DB2 Employee Service
```
Endpoint: /employees/*
Target: http://localhost:3001
Transformations: REST ↔ SQL
```

### IMS Phonebook Service
```
Endpoint: /phonebook/*
Target: http://localhost:3002
Transformations: REST ↔ IMS Segments
```

## API Endpoints

### Documentation
```
GET /
Returns: API documentation and available endpoints
```

### Metrics
```
GET /metrics
Returns: Service usage statistics and performance metrics
```

## Data Transformations

### CICS Transformations
```javascript
// Example REST to COMMAREA
REST Request:
POST /catalog/orders
{
    "itemId": "1001",
    "quantity": 5
}

→ Transforms to COMMAREA format:
01 ORDER-AREA.
   03 ITEM-ID   PIC X(4).
   03 QUANTITY  PIC 9(4).
```

### DB2 Transformations
```javascript
// Example REST to SQL
REST Request:
GET /employees?department=A00

→ Transforms to SQL:
SELECT * FROM EMPLOYEE 
WHERE WORKDEPT = 'A00'
```

### IMS Transformations
```javascript
// Example REST to IMS
REST Request:
POST /phonebook/contacts
{
    "firstName": "John",
    "lastName": "Smith"
}

→ Transforms to IMS Segment:
CONTACT01SMITH    JOHN    
```

## Configuration

### Environment Variables
```
PORT=3000 (default)
CICS_SERVICE_URL=http://localhost:3000
DB2_SERVICE_URL=http://localhost:3001
IMS_SERVICE_URL=http://localhost:3002
```

### Monitoring Configuration
```javascript
// Available metrics
{
    requests: {
        total: Number,
        byService: {
            cics: Number,
            db2: Number,
            ims: Number
        }
    },
    responseTime: {
        average: Number
    }
}
```

## Installation

1. Install dependencies:
```bash
npm install
```

2. Start the server:
```bash
npm start
```

## Usage Examples

### CICS Catalog API
```bash
# Get all items
curl http://localhost:3000/catalog/items

# Get specific item
curl http://localhost:3000/catalog/items/1001

# Place order
curl -X POST "http://localhost:3000/catalog/orders?itemNumber=1001&quantity=5"
```

### DB2 Employee API
```bash
# Get all employees
curl http://localhost:3000/employees

# Get employee by ID
curl http://localhost:3000/employees/000010
```

### IMS Phonebook API
```bash
# Get contact
curl http://localhost:3000/phonebook/contacts/Smith
```

## Development

### Project Structure
```
mock-api-gateway/
├── server.js          # Main application file
├── monitoring.js      # Monitoring implementation
├── package.json       # Dependencies and scripts
└── Dockerfile        # Container configuration
```

### Adding New Features
1. **New Proxy Route**:
```javascript
const newProxy = createProxyMiddleware('/path', {
    target: 'http://service-url',
    pathRewrite: {'^/path': ''},
    changeOrigin: true
});
app.use('/path', newProxy);
```

2. **New Transformation**:
```javascript
onProxyReq: (proxyReq, req, res) => {
    // Add transformation logic
}
```

## Testing
```bash
# Check service health
curl http://localhost:3000/

# Monitor metrics
curl http://localhost:3000/metrics
```

## Docker Support

### Build
```bash
docker build -t mock-api-gateway .
```

### Run
```bash
docker run -p 3000:3000 mock-api-gateway
```

## Kubernetes/OpenShift Deployment
Refer to [DEPLOYMENT.md](../DEPLOYMENT.md) for detailed deployment instructions.

## Error Handling
The gateway handles various error scenarios:
- Service unavailable
- Transformation errors
- Invalid requests
- Timeout handling

## Monitoring and Logging
- Request/Response logging
- Service metrics
- Performance monitoring
- Error tracking

## Security Considerations
- CORS enabled
- Basic request validation
- Service isolation

## Contributing
1. Fork the repository
2. Create your feature branch
3. Commit your changes
4. Push to the branch
5. Create a Pull Request

## License
This project is licensed under the MIT License - see the LICENSE file for details.