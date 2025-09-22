# CICS Data Flow and Mapping Guide

## 1. Complete Request Flow

```
Client Request → API Gateway → CICS Service → CICS Program
     ↓              ↓             ↓              ↓
JSON/REST    Route to CICS   Transform     COMMAREA
     ↓              ↓             ↓              ↓
  Request     Transform     COMMAREA      Process Data
     ↓              ↓             ↓              ↓
  Response  ←   JSON    ← COMMAREA    ←  COBOL Data
```

## 2. Detailed Flow Example

### Step 1: Client REST Request
```http
POST /catalog/orders
{
    "itemId": "1001",
    "quantity": 5
}
```

### Step 2: API Gateway Processing
```javascript
// In API Gateway
const cicsProxy = createProxyMiddleware('/catalog', {
    onProxyReq: (proxyReq, req, res) => {
        if (req.method === 'POST' && req.path.includes('/orders')) {
            // Transform JSON to COMMAREA format
        }
    }
});
```

### Step 3: COMMAREA Structure
```cobol
01 DFHCOMMAREA.
   03 CA-REQUEST-ID     PIC X(6).
   03 CA-RETURN-CODE    PIC 9(2).
   03 CA-RESPONSE-MSG   PIC X(79).
   03 CA-ORDER-DATA.
      05 CA-ITEM-REF    PIC 9(4).
      05 CA-QUANTITY    PIC 9(3).
```

### Step 4: Data Transformation
```javascript
// JSON to COMMAREA Transformation
const transformOrderData = (jsonData) => {
    const commarea = {
        requestId: 'ORDER1',
        returnCode: '00',
        responseMsg: ' '.repeat(79),
        orderData: {
            itemRef: jsonData.itemId.padStart(4, '0'),
            quantity: String(jsonData.quantity).padStart(3, '0')
        }
    };
    return formatCommarea(commarea);
};
```

## 3. COMMAREA to JSON Mappings

### Basic Types
```
COBOL COMMAREA         →  JSON/JavaScript
------------------------------------------
PIC X(n)              →  String (trimmed)
PIC 9(n)              →  Number/String
PIC S9(n)             →  Number
GROUP items           →  Object
OCCURS n TIMES        →  Array
```

### Example: Catalog Item
```cobol
03 CA-ITEM-DETAILS.
   05 CA-ITEM-REF     PIC 9(4).
   05 CA-DESCRIPTION  PIC X(40).
   05 CA-DEPARTMENT   PIC 9(3).
   05 CA-COST         PIC X(6).
   05 IN-STOCK        PIC 9(4).
   05 ON-ORDER        PIC 9(3).
```
```javascript
{
    "itemReference": "1001",
    "description": "Blue Pen",
    "department": 10,
    "cost": "2.50",
    "inStock": 100,
    "onOrder": 50
}
```

## 4. Real-World CICS Flow

```
1. Client Request
   POST /catalog/orders

2. z/OS Connect
   - Authenticates request
   - Builds COMMAREA
   - Calls CICS program

3. CICS Program
   - Receives COMMAREA
   - Processes request
   - Updates COMMAREA

4. z/OS Connect
   - Reads COMMAREA
   - Transforms to JSON
   - Sends response
```

## 5. Common CICS Scenarios

### Scenario 1: Get Item Details
```http
GET /catalog/items/1001
```
```cobol
* COMMAREA Input
03 CA-REQUEST-ID     PIC X(6) VALUE 'GETITM'.
03 CA-ITEM-REF-REQ  PIC 9(4) VALUE 1001.

* COMMAREA Output
03 CA-ITEM-DETAILS.
   05 CA-ITEM-REF    PIC 9(4).
   05 CA-DESCRIPTION PIC X(40).
   05 CA-DEPARTMENT  PIC 9(3).
   05 CA-COST        PIC X(6).
```
```javascript
// Final JSON Response
{
    "itemReference": "1001",
    "description": "Blue Pen",
    "department": "10",
    "cost": "2.50"
}
```

### Scenario 2: Place Order
```http
POST /catalog/orders
{
    "itemId": "1001",
    "quantity": 5
}
```
```cobol
* COMMAREA Input
03 CA-REQUEST-ID     PIC X(6) VALUE 'ORDER1'.
03 CA-ORDER-DATA.
   05 CA-ITEM-REF    PIC 9(4) VALUE 1001.
   05 CA-QUANTITY    PIC 9(3) VALUE 005.
```

## 6. Error Handling

### CICS Return Codes
```javascript
// CICS Response Code Mapping
const cicsResponseMapping = {
    '00': { status: 200, message: 'Success' },
    '01': { status: 404, message: 'Item not found' },
    '02': { status: 400, message: 'Invalid quantity' },
    '03': { status: 409, message: 'Insufficient stock' }
};
```

### Data Validation
```javascript
const validateOrder = (data) => {
    if (!data.itemId || !/^\d{4}$/.test(data.itemId)) {
        throw new Error('Invalid item ID format');
    }
    if (!data.quantity || data.quantity < 1) {
        throw new Error('Invalid quantity');
    }
};
```

## 7. CICS-Specific Considerations

1. **State Management**:
   - CICS maintains transaction state
   - Each request is independent
   - COMMAREA size limitations

2. **Data Padding**:
   - Fixed-length fields
   - Right-pad with spaces
   - Zero-pad numeric fields

3. **Character Encoding**:
   - EBCDIC to ASCII conversion
   - Special character handling
   - Numeric format preservation
