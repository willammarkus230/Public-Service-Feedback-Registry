# 🏛️ Public Service Feedback Registry 

A decentralized application for recording and managing immutable feedback for civil servants on the Stacks blockchain.

## 🎯 Features

- ✨ Add and manage civil servant profiles
- 📝 Submit public feedback and ratings
- 🔢 Automatic calculation of performance metrics
- 🔒 Immutable and transparent feedback history
- 👮 Administrative controls for profile management

## 📚 Contract Functions

### Administrative Functions

- `add-civil-servant`: Add a new civil servant to the registry
- `deactivate-servant`: Deactivate a civil servant profile
- `transfer-admin`: Transfer administrative privileges

### Public Functions

- `submit-feedback`: Submit feedback for a civil servant (score 1-5 and comment)
- `get-civil-servant`: View civil servant details
- `get-feedback`: View specific feedback
- `get-servant-stats`: View performance statistics

## 🚀 Usage

1. Deploy the contract using Clarinet
2. Initialize civil servant profiles using the admin account
3. Public can submit feedback using their STX accounts
4. View feedback and statistics using read-only functions

## ⚠️ Requirements

- Clarinet
- Stacks wallet for interaction
- Admin privileges for management functions

## 🔍 Validation Rules

- Scores must be between 1 and 5
- Each address can only submit feedback once per civil servant
- Only active civil servants can receive feedback
- Administrative functions restricted to admin account


