# CICS Catalog Service Mock

## Overview
This service simulates a CICS-based catalog system, demonstrating how traditional CICS transactions can be exposed as modern REST APIs.

## CICS Concepts Demonstrated
- COMMAREA data structures
- Transaction processing
- State management
- Data transformation

## API Endpoints

### Get All Items
```
GET /items
Query Parameters:
- startItemID: Starting item reference number

Response:
{
    "totalItems": number,
    "items": [CatalogItem]
}
```

### Get Specific Item
```
GET /items/{id}
Parameters:
- id: Item reference number

Response:
{
    "information": {
        "itemReference": number,
        "description": string,
        "cost": string,
        ...
    }
}
```

### Place Order
```
POST /orders
Query Parameters:
- itemNumber: Item reference
- quantity: Order quantity

Response:
{
    "message": "Order confirmation"
}
```

## Data Structures

### COMMAREA to JSON Mapping
```
COBOL Structure:
01 DFH0XCP1.
   03 CA-REQUEST-ID     PIC X(6).
   03 CA-RETURN-CODE    PIC 9(2).
   03 CA-RESPONSE-MSG   PIC X(79).
   03 CA-REQUEST-SPECIFIC...

↓ Transforms to ↓

JSON Structure:
{
    "requestId": string,
    "returnCode": number,
    "responseMessage": string,
    ...
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

## Configuration

### Environment Variables
```
PORT=3000 (default)
```

## Usage Examples

### Get All Items
```bash
curl http://localhost:3000/items
```

### Get Specific Item
```bash
curl http://localhost:3000/items/1001
```

### Place Order
```bash
curl -X POST "http://localhost:3000/orders?itemNumber=1001&quantity=5"
```

## Development

### Project Structure
```
simple-cics-mock-api/
├── server.js       # Main application file
├── mockData.js     # Simulated catalog data
├── package.json    # Dependencies and scripts
└── Dockerfile     # Container configuration
```

### Mock Data Format
```javascript
{
    "itemReference": "1001",
    "description": "Item Description",
    "cost": "10.00",
    "department": "A00",
    "stock": 100,
    "onOrder": 10
}
```

## Testing
```bash
# Test service health
curl http://localhost:3000/

# Get all items
curl http://localhost:3000/items
```

## Docker Support

### Build
```bash
docker build -t cics-mock-api .
```

### Run
```bash
docker run -p 3000:3000 cics-mock-api
```

## Error Handling
- Invalid item references
- Out of stock conditions
- Invalid order quantities
- System errors

## Integration
This service is designed to be used with the API Gateway but can also be used standalone.

## CICS Learning Resources
- [IBM CICS Documentation](https://www.ibm.com/docs/en/cics-ts)
- [CICS Transaction Processing](https://www.ibm.com/docs/en/cics-ts/5.6?topic=processing-about-cics-transaction-server)
- [COMMAREA Programming](https://www.ibm.com/docs/en/cics-ts/5.6?topic=programming-commarea)
