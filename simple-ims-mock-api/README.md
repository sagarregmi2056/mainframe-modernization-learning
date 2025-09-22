# IMS Phonebook Service Mock

## Overview
This service simulates an IMS-based phonebook system, demonstrating how hierarchical IMS databases can be exposed as REST APIs.

## IMS Concepts Demonstrated
- Hierarchical data structures
- Segment-based operations
- Key-based access
- Parent-child relationships

## API Endpoints

### Add Contact
```
POST /phonebook/contacts
Body: {
    "firstName": string,
    "lastName": string,
    "extension": string,
    "zipCode": string
}

Response:
{
    Contact details
}
```

### Get Contact
```
GET /phonebook/contacts/{lastName}
Parameters:
- lastName: Contact's last name

Response:
{
    "firstName": string,
    "lastName": string,
    "extension": string,
    "zipCode": string
}
```

### Update Contact
```
PUT /phonebook/contacts/{lastName}
Parameters:
- lastName: Contact's last name
Body: Updated contact information
```

### Delete Contact
```
DELETE /phonebook/contacts/{lastName}
Parameters:
- lastName: Contact's last name
```

## Data Structures

### IMS to JSON Mapping
```
IMS Segment Structure:
CONTACT-SEG
- LAST-NAME  CHAR(10)
- FIRST-NAME CHAR(10)
- EXTENSION  CHAR(10)
- ZIP-CODE   CHAR(7)

↓ Transforms to ↓

JSON Structure:
{
    "lastName": string,
    "firstName": string,
    "extension": string,
    "zipCode": string
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
PORT=3002 (default)
```

## Usage Examples

### Add New Contact
```bash
curl -X POST http://localhost:3002/phonebook/contacts \
  -H "Content-Type: application/json" \
  -d '{
    "firstName": "John",
    "lastName": "Smith",
    "extension": "555-0101",
    "zipCode": "12345"
  }'
```

### Get Contact
```bash
curl http://localhost:3002/phonebook/contacts/Smith
```

### Update Contact
```bash
curl -X PUT http://localhost:3002/phonebook/contacts/Smith \
  -H "Content-Type: application/json" \
  -d '{
    "extension": "555-0102"
  }'
```

## Development

### Project Structure
```
simple-ims-mock-api/
├── server.js       # Main application file
├── mockData.js     # Simulated contact data
├── package.json    # Dependencies and scripts
└── Dockerfile     # Container configuration
```

### Mock Data Format
```javascript
{
    "firstName": "John",
    "lastName": "Smith",
    "extension": "555-0101",
    "zipCode": "12345"
}
```

## Testing
```bash
# Test service health
curl http://localhost:3002/

# Get contact
curl http://localhost:3002/phonebook/contacts/Smith
```

## Docker Support

### Build
```bash
docker build -t ims-mock-api .
```

### Run
```bash
docker run -p 3002:3002 ims-mock-api
```

## Error Handling
- Duplicate contacts
- Contact not found
- Invalid data format
- System errors

## Integration
This service is designed to be used with the API Gateway but can also be used standalone.

## IMS Learning Resources
- [IBM IMS Documentation](https://www.ibm.com/docs/en/ims)
- [IMS Database Administration](https://www.ibm.com/docs/en/ims/15.2.0?topic=administration-database)
- [IMS Application Programming](https://www.ibm.com/docs/en/ims/15.2.0?topic=programming-application)

## Field Validations
- firstName: max length 10 characters
- lastName: max length 10 characters
- extension: max length 10 characters
- zipCode: max length 7 characters

## Hierarchical Data Example
```
Root (Contact)
└── Last Name (Key)
    ├── First Name
    ├── Extension
    └── Zip Code
```

## Performance Considerations
- In-memory data storage for demonstration
- Simulated hierarchical access
- Fast key-based lookups
