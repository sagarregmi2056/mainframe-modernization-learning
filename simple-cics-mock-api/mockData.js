// Mock data representing catalog items
const catalogItems = [
    {
        information: {
            itemReference: 1001,
            description: "Ball Pens Red 24pk",
            cost: "2.90",
            department: 10,
            stock: 56,
            onOrder: 10
        },
        summary: {
            stock: "Department 10 has 56 'Ball Pens Red 24pk' in stock.",
            orders: "10 'Ball Pens Red 24pk' on order at price $2.90. Total orders value: $29.00"
        }
    },
    {
        information: {
            itemReference: 1002,
            description: "Notepad A4 50 sheets",
            cost: "3.50",
            department: 10,
            stock: 100,
            onOrder: 20
        },
        summary: {
            stock: "Department 10 has 100 'Notepad A4 50 sheets' in stock.",
            orders: "20 'Notepad A4 50 sheets' on order at price $3.50. Total orders value: $70.00"
        }
    }
];

module.exports = {
    catalogItems
};
