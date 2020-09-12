package account

import "sync"

// Account is an account
type Account struct {
	deposit int64
	closed  bool
	mutex   *sync.Mutex
}

// Open opens an account
func Open(initialDeposit int64) *Account {
	if initialDeposit < 0 {
		return nil
	}

	return &Account{initialDeposit, false, &sync.Mutex{}}
}

// Balance returns its balance
func (a *Account) Balance() (balance int64, ok bool) {
	if a.closed {
		return 0, false
	}

	return a.deposit, true
}

// Close closes an account
func (a *Account) Close() (payout int64, ok bool) {
	a.mutex.Lock()
	defer a.mutex.Unlock()

	if a.closed {
		return 0, false
	}
	a.closed = true

	return a.deposit, true
}

// Deposit deposit the given money
func (a *Account) Deposit(amount int64) (int64, bool) {
	if a.closed {
		return 0, false
	}

	a.mutex.Lock()
	defer a.mutex.Unlock()

	newBalance := a.deposit + amount
	if newBalance < 0 {
		return 0, false
	}
	a.deposit += amount

	return a.deposit, true
}
