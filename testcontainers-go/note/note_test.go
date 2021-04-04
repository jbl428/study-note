package note

import (
	"context"
	"fmt"
	"os"
	"reflect"
	"testing"

	"testcontainers/db"

	"github.com/testcontainers/testcontainers-go"
	"github.com/testcontainers/testcontainers-go/wait"
)

var (
	user     = "tester"
	password = "tester"
	host     = "127.0.0.1"
	database = "containers"
)

func TestMain(m *testing.M) {
	os.Setenv("DB_HOST", host)
	os.Setenv("DB_USER", user)
	os.Setenv("DB_PASSWORD", password)
	os.Setenv("DB_DATABASE", database)

	workingDir, _ := os.Getwd()
	mountFrom := fmt.Sprintf("%s/../scripts/init.sql", workingDir)
	print(workingDir)
	mountTo := "/docker-entrypoint-initdb.d/init.sql"

	ctx := context.Background()
	req := testcontainers.ContainerRequest{
		Image:        "mysql:latest",
		ExposedPorts: []string{"3306/tcp"},
		BindMounts:   map[string]string{mountFrom: mountTo},
		Env: map[string]string{
			"MYSQL_RANDOM_ROOT_PASSWORD": "yes",
			"MYSQL_USER":                 user,
			"MYSQL_PASSWORD":             password,
			"MYSQL_DATABASE":             database,
		},
		WaitingFor: wait.ForLog("port: 3306  MySQL Community Server - GPL"),
	}
	mysqlContainer, err := testcontainers.GenericContainer(ctx, testcontainers.GenericContainerRequest{
		ContainerRequest: req,
		Started:          true,
	})
	if err != nil {
		panic(err)
	}
	defer mysqlContainer.Terminate(ctx)

	p, _ := mysqlContainer.MappedPort(ctx, "3306")
	os.Setenv("DB_PORT", p.Port())
	db.Init()

	exitVal := m.Run()
	os.Exit(exitVal)
}

func TestGetNotes(t *testing.T) {
	type args struct {
		isVerified bool
	}
	tests := []struct {
		name    string
		args    args
		want    []*Note
		wantErr bool
	}{
		{
			name: "verified",
			args: args{isVerified: true},
			want: []*Note{
				{ID: 1, Content: "note 1"},
				{ID: 3, Content: "note 3"},
			},
			wantErr: false,
		},
		{
			name: "not verified",
			args: args{isVerified: false},
			want: []*Note{
				{ID: 2, Content: "note 2"},
			},
			wantErr: false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := GetNotes(tt.args.isVerified)
			if (err != nil) != tt.wantErr {
				t.Errorf("GetNotes() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !reflect.DeepEqual(got, tt.want) {
				t.Errorf("GetNotes() got = %v, want %v", got, tt.want)
			}
		})
	}
}
