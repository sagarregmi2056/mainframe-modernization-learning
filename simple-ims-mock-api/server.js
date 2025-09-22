const express = require('express');
const cors = require('cors');
const swaggerUi = require('swagger-ui-express');
const { contacts } = require('./mockData');

const app = express();
const port = 3002; 


app.use(cors());
app.use(express.json());

// POST /phonebook/contacts
app.post('/phonebook/contacts', (req, res) => {
    const newContact = req.body;
    
    // Validate required fields
    if (!newContact.firstName || !newContact.lastName || 
        !newContact.extension || !newContact.zipCode) {
        return res.status(400).json({
            message: "Missing required fields"
        });
    }
    
    // Check for duplicate lastName (unique key in IMS)
    if (contacts.find(c => c.lastName === newContact.lastName)) {
        return res.status(409).json({
            message: "Contact with this last name already exists"
        });
    }
    
   
    if (newContact.firstName.length > 10 || 
        newContact.lastName.length > 10 ||
        newContact.extension.length > 10 ||
        newContact.zipCode.length > 7) {
        return res.status(400).json({
            message: "Field length exceeds maximum allowed"
        });
    }
    
    contacts.push(newContact);
    res.status(201).json(newContact);
});

// GET /phonebook/contacts/{lastName} 
app.get('/phonebook/contacts/:lastName', (req, res) => {
    const contact = contacts.find(c => c.lastName === req.params.lastName);
    
    if (!contact) {
        return res.status(404).json({
            message: "Contact could not be found"
        });
    }
    
    res.json(contact);
});

// PUT /phonebook/contacts/{lastName} 
app.put('/phonebook/contacts/:lastName', (req, res) => {
    const contactIndex = contacts.findIndex(c => c.lastName === req.params.lastName);
    
    if (contactIndex === -1) {
        return res.status(404).json({
            message: "Contact could not be found"
        });
    }
    
    // Validate field lengths
    if ((req.body.firstName && req.body.firstName.length > 10) || 
        (req.body.extension && req.body.extension.length > 10) ||
        (req.body.zipCode && req.body.zipCode.length > 7)) {
        return res.status(400).json({
            message: "Field length exceeds maximum allowed"
        });
    }
    
    // Update contact while preserving lastName
    contacts[contactIndex] = {
        ...contacts[contactIndex],
        ...req.body,
        lastName: contacts[contactIndex].lastName // Ensure lastName doesn't change
    };
    
    res.json(contacts[contactIndex]);
});

// DELETE /phonebook/contacts/{lastName} - Delete contact
app.delete('/phonebook/contacts/:lastName', (req, res) => {
    const contactIndex = contacts.findIndex(c => c.lastName === req.params.lastName);
    
    if (contactIndex === -1) {
        return res.status(404).json({
            message: "Contact could not be found"
        });
    }
    
    const deletedContact = contacts.splice(contactIndex, 1)[0];
    res.json(deletedContact);
});

// Basic API documentation endpoint
app.get('/', (req, res) => {
    res.json({
        message: "IMS Phonebook API Mock",
        endpoints: {
            "POST /phonebook/contacts": "Add new contact",
            "GET /phonebook/contacts/:lastName": "Get contact by last name",
            "PUT /phonebook/contacts/:lastName": "Update contact",
            "DELETE /phonebook/contacts/:lastName": "Delete contact"
        }
    });
});

// Start server
app.listen(port, () => {
    console.log(`IMS Mock API server running at http://localhost:${port}`);
});
