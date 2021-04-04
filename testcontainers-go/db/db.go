package db

import (
	"database/sql"
	"fmt"
	"log"
	"os"
	"sync"

	_ "github.com/go-sql-driver/mysql"
)

var (
	DB   *sql.DB
	once sync.Once
)

// Init initialize one time settings for database connection
func Init() {
	once.Do(func() {
		var err error
		DB, err = sql.Open(
			"mysql",
			fmt.Sprintf(
				"%s:%s@tcp(%s:%s)/%s?parseTime=true&timeout=5s",
				os.Getenv("DB_USER"),
				os.Getenv("DB_PASSWORD"),
				os.Getenv("DB_HOST"),
				os.Getenv("DB_PORT"),
				os.Getenv("DB_DATABASE"),
			),
		)
		if err != nil {
			log.Fatalf("sql.Open: %v", err)
		}
	})
}
