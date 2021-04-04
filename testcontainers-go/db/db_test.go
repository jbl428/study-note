package db_test

import (
	"testing"

	"testcontainers/db"
)

func TestInit(t *testing.T) {
	db.Init()

	if db.DB == nil {
		t.Error("failed to init db")
	}
	defer db.DB.Close()

	tempDB := db.DB
	db.Init()

	if db.DB != tempDB {
		t.Error("db should be initialized once")
	}
}
