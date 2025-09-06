# 🧾 Invox - P2P Invoicing Protocol

A decentralized invoicing system built on Stacks blockchain that enables freelancers to create, manage, and receive payments for invoices in a trustless manner.

## 🚀 Features

- Create invoices with custom amounts and due dates
- Pay invoices using STX tokens
- Cancel pending invoices
- Track payment status and history
- View invoices by creator or payer

## 💻 Usage

### Creating an Invoice

```clarity
(contract-call? .invox create-invoice 
    'SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9 
    u1000 
    u100 
    "Website Development"
)
```

### Paying an Invoice

```clarity
(contract-call? .invox pay-invoice 
    u1 
    "tx-hash-example"
)
```

### Cancelling an Invoice

```clarity
(contract-call? .invox cancel-invoice u1)
```

### Viewing Invoice Details

```clarity
(contract-call? .invox get-invoice u1)
```

## 🔧 Installation

1. Clone the repository
2. Install Clarinet
3. Run `clarinet console` to interact with the contract

## 🤝 Contributing

Feel free to open issues and submit pull requests!

## 📜 License

MIT



