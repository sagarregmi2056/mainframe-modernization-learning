# IMS Data Flow and Mapping Guide

## 1. Complete Request Flow

```
Client Request → API Gateway → IMS Service → IMS Database
     ↓              ↓             ↓             ↓
JSON/REST    Route to IMS    Transform     IMS Segment
     ↓              ↓             ↓             ↓
  Request     Transform      DL/I Call    Process Data
     ↓              ↓             ↓             ↓
  Response  ←   JSON     ←   Segment   ←  COBOL Data
```

## 2. Detailed Flow Example

### Step 1: Client REST Request
```http
POST /phonebook/contacts
{
    "firstName": "John",
    "lastName": "Smith",
    "extension": "555-0101",
    "zipCode": "12345"
}
```

### Step 2: API Gateway Processing
```javascript
// In API Gateway
const imsProxy = createProxyMiddleware('/phonebook', {
    onProxyReq: (proxyReq, req, res) => {
        if (req.method === 'POST') {
            // Transform JSON to IMS segment format
        }
    }
});
```

### Step 3: IMS Segment Structure
```cobol
01 CONTACT-SEGMENT.
   02 LAST-NAME   PIC X(10).
   02 FIRST-NAME  PIC X(10).
   02 EXTENSION   PIC X(10).
   02 ZIP-CODE    PIC X(7).
```

### Step 4: Data Transformation
```javascript
// JSON to IMS Segment Transformation
const transformContactData = (jsonData) => {
    const segment = {
        lastName: jsonData.lastName.padEnd(10, ' '),
        firstName: jsonData.firstName.padEnd(10, ' '),
        extension: jsonData.extension.padEnd(10, ' '),
        zipCode: jsonData.zipCode.padEnd(7, ' ')
    };
    return formatSegment(segment);
};
```

## 3. IMS to JSON Mappings

### Basic Types
```
IMS Segment Field     →  JSON/JavaScript
------------------------------------------
PIC X(n)              →  String (trimmed)
PIC 9(n)              →  Number/String
PIC S9(n)             →  Number
Child Segments        →  Nested Objects/Arrays
```

### Example: Contact Record
```cobol
* Root Segment
01 CONTACT-SEGMENT.
   02 LAST-NAME   PIC X(10).
   02 FIRST-NAME  PIC X(10).
   02 EXTENSION   PIC X(10).
   02 ZIP-CODE    PIC X(7).

* Child Segment (if any)
01 ADDRESS-SEGMENT.
   02 STREET     PIC X(30).
   02 CITY       PIC X(20).
   02 STATE      PIC X(2).
```
```javascript
{
    "lastName": "Smith",
    "firstName": "John",
    "extension": "555-0101",
    "zipCode": "12345",
    "address": {
        "street": "123 Main St",
        "city": "Springfield",
        "state": "IL"
    }
}
```

## 4. Real-World IMS Flow

```
1. Client Request
   POST /phonebook/contacts

2. z/OS Connect
   - Authenticates request
   - Builds IMS segment
   - Issues DL/I call

3. IMS Database
   - Processes segment
   - Updates database
   - Returns result

4. z/OS Connect
   - Reads segment
   - Transforms to JSON
   - Sends response
```

## 5. Common IMS Operations

### Get Contact (GU - Get Unique)
```http
GET /phonebook/contacts/Smith
```
```cobol
* IMS DL/I Call
CALL 'CBLTDLI' USING GU,
                     PCB-PHONE,
                     CONTACT-SEGMENT,
                     SSA-CONTACT.
```
```javascript
// Final JSON Response
{
    "lastName": "Smith",
    "firstName": "John",
    "extension": "555-0101",
    "zipCode": "12345"
}
```

### Add Contact (ISRT - Insert)
```http
POST /phonebook/contacts
```
```cobol
* IMS DL/I Call
CALL 'CBLTDLI' USING ISRT,
                     PCB-PHONE,
                     CONTACT-SEGMENT.
```

## 6. IMS-Specific Considerations

1. **Hierarchical Structure**:
   ```
   Root (Contact)
   └── Last Name (Key)
       ├── First Name
       ├── Extension
       └── Zip Code
   ```

2. **Segment Types**:
   - Root segments
   - Dependent segments
   - Virtual segments

3. **DL/I Commands**:
   - GU (Get Unique)
   - GN (Get Next)
   - ISRT (Insert)
   - REPL (Replace)
   - DLET (Delete)

## 7. Error Handling

### IMS Status Codes
```javascript
const imsStatusMapping = {
    'QC': { status: 404, message: 'Segment not found' },
    'II': { status: 409, message: 'Duplicate segment' },
    'AM': { status: 403, message: 'Access denied' }
};
```

### Data Validation
```javascript
const validateContact = (data) => {
    if (!data.lastName || data.lastName.length > 10) {
        throw new Error('Invalid last name');
    }
    if (!data.firstName || data.firstName.length > 10) {
        throw new Error('Invalid first name');
    }
    // ... more validations
};
```

## 8. Key Differences from Other Systems

1. **Hierarchical vs Relational**:
   - IMS: Parent-child relationships
   - DB2: Tables and joins

2. **Access Methods**:
   - IMS: Sequential and direct
   - DB2: SQL-based access

3. **Data Organization**:
   - IMS: Segments and hierarchies
   - CICS: Flat COMMAREA structure
