# 📚 Decentralized Book Publishing & Royalties

> 🎯 **Problem:** Authors get low royalties from traditional publishers  
> ✨ **Solution:** A decentralized platform where readers pay authors directly via smart contracts

## 🌟 Features

- 📖 **Direct Publishing:** Authors can publish books directly on the blockchain
- 💰 **Instant Royalties:** Authors receive payments immediately when books are purchased
- 🔒 **Ownership Control:** Authors maintain full control over their work
- ⭐ **Review System:** Readers can rate and review purchased books
- 💎 **Transparent Fees:** Low platform fees with full transparency
- 🛡️ **Secure Payments:** All transactions handled by smart contracts

## 🚀 Getting Started

### Prerequisites
- [Clarinet CLI](https://github.com/hirosystems/clarinet) installed
- [Stacks Wallet](https://leather.io/) for transactions

### Installation

1. Clone the repository:
```bash
git clone <your-repo-url>
cd Decentralized-Book-Publishing---Royalties
```

2. Install dependencies:
```bash
npm install
```

3. Check the contract:
```bash
clarinet check
```

4. Run tests:
```bash
npm test
```

## 📋 Contract Functions

### 📚 Publishing Functions

#### `publish-book`
Publish a new book to the platform.

```clarity
(publish-book "My Awesome Book" u1000000 u500 "hash123...")
```
- `title`: Book title (max 100 characters)
- `price`: Price in microSTX (1 STX = 1,000,000 microSTX)
- `royalty-rate`: Royalty percentage (basis points, 500 = 5%)
- `content-hash`: IPFS or content hash for the book

#### `update-book-price`
Update the price of your published book.

```clarity
(update-book-price u1 u2000000)
```

### 💰 Purchase Functions

#### `purchase-book`
Purchase a book and gain access to its content.

```clarity
(purchase-book u1)
```

### ⭐ Review Functions

#### `add-review`
Add a review for a purchased book (1-5 stars).

```clarity
(add-review u1 u5 "Amazing book, highly recommended!")
```

### 🔍 Read-Only Functions

#### `get-book`
Get book information by ID.

```clarity
(get-book u1)
```

#### `has-purchased`
Check if a user has purchased a specific book.

```clarity
(has-purchased 'SP1234... u1)
```

#### `get-author-stats`
Get statistics for an author.

```clarity
(get-author-stats 'SP1234...)
```

#### `get-book-ratings`
Get rating statistics for a book.

```clarity
(get-book-ratings u1)
```

## 💡 Usage Examples

### 📖 For Authors

1. **Publish a Book:**
```bash
# Connect your wallet and call:
(contract-call? .Decentralized-Book-Publishing publish-book 
  "The Future of Web3" 
  u5000000  ; 5 STX
  u750      ; 7.5% royalty
  "QmXyz123abcdef...")
```

2. **Check Your Earnings:**
```bash
(contract-call? .Decentralized-Book-Publishing get-author-stats tx-sender)
```

### 📚 For Readers

1. **Purchase a Book:**
```bash
(contract-call? .Decentralized-Book-Publishing purchase-book u1)
```

2. **Leave a Review:**
```bash
(contract-call? .Decentralized-Book-Publishing add-review 
  u1 
  u4 
  "Great insights on blockchain technology!")
```

## 🏗️ Architecture

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│     Authors     │    │   Smart Contract │    │     Readers     │
│                 │    │                 │    │                 │
│ • Publish books │◄──►│ • Store metadata│◄──►│ • Buy books     │
│ • Set prices    │    │ • Handle payments│    │ • Leave reviews │
│ • Earn royalties│    │ • Manage reviews │    │ • Rate content  │
└─────────────────┘    └─────────────────┘    └─────────────────┘
```

## 💰 Economics

- **Platform Fee:** 2.5% (adjustable by contract owner)
- **Author Earnings:** 97.5% of book price (minus platform fee)
- **Gas Fees:** Standard Stacks transaction fees apply

## 🔧 Testing

Run the test suite:

```bash
npm test
```

The tests cover:
- ✅ Book publishing and metadata storage
- ✅ Purchase transactions and access control
- ✅ Review and rating system
- ✅ Author earnings and statistics
- ✅ Error handling and edge cases

## 🛡️ Security Features

- 🔐 **Access Control:** Only book owners can update prices
- 💸 **Payment Protection:** Smart contract handles all transactions
- 🚫 **Duplicate Prevention:** Prevents duplicate purchases and reviews
- ✅ **Input Validation:** All inputs are validated for safety
- 🔍 **Transparent Operations:** All transactions are publicly verifiable

## 🌍 Deployment

### Testnet Deployment
```bash
clarinet deploy --testnet
```

### Mainnet Deployment
```bash
clarinet deploy --mainnet
```

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 🔗 Links

- [Stacks Documentation](https://docs.stacks.co/)
- [Clarinet Documentation](https://docs.hiro.so/stacks/clarinet)
- [Clarity Language Reference](https://docs.stacks.co/reference/functions)

---

**Made with ❤️ for the decentralized web**
