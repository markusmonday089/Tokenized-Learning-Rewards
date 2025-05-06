# Learning Rewards Smart Contract

A Clarity smart contract that implements a tokenized learning rewards system on Stacks blockchain. Users can earn tokens by completing learning milestones.

## Features

- Fungible token rewards for completing learning milestones
- Milestone management system
- User progress tracking
- Token transfer capabilities
- Configurable milestone points and difficulty levels

## Contract Functions

### Administrative Functions
- `set-milestone`: Create or update milestone details
- `set-token-uri`: Update token metadata URI
- `set-contract-owner`: Transfer contract ownership

### User Functions
- `claim-milestone`: Claim rewards for completing a milestone
- `transfer-tokens`: Transfer earned tokens to other users
- `get-user-progress`: View user's completed milestones and points
- `get-milestone`: View milestone details
- `has-claimed-milestone`: Check if user has claimed specific milestone

## Usage

1. Deploy contract using Clarinet or Stacks wallet
2. Set up milestones using `set-milestone`
3. Users can claim milestones using `claim-milestone`
4. Earned tokens can be transferred using `transfer-tokens`

## Testing

Use Clarinet to run tests:
```bash
clarinet test
```