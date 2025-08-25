Lending-STX
A decentralized lending and borrowing protocol built on the Stacks blockchain using Clarity smart contracts.
This project enables users to deposit STX as collateral, borrow assets, repay loans, and withdraw collateral securely.

Features
Deposit STX as collateral
Borrow against collateral with collateral ratio enforcement
Repay borrowed amounts with interest
Withdraw collateral after repayment
Interest rate model implementation
Event logging for transparency

Technical Overview
Language: Clarity (Stacks smart contract language)
Functions: deposit, borrow, repay, withdraw
Security: Collateralization checks + access control
Planned Additions: liquidation mechanism, governance module, support for multiple assets

Installation & Usage
Clone the repository:
git clone https://github.com/your-repo/lending-stx.git
cd lending-stx
Deploy the smart contract using Clarinet
:
clarinet contract deploy lending-stx


Run tests:
clarinet test

Roadmap
Add liquidation system for undercollateralized positions
Integrate governance for dynamic interest rates
Expand collateral support beyond STX
Security audit and optimization

License
MIT License – free to use, modify, and distribute.
