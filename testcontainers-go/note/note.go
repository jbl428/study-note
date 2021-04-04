package note

import "testcontainers/db"

type Note struct {
	ID      int
	Content string
}

// GetNotes returns notes that the given condition match
func GetNotes(isVerified bool) ([]*Note, error) {
	rows, err := db.DB.Query("SELECT * FROM notes WHERE isVerified = ?", isVerified)
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	var notes []*Note
	for rows.Next() {
		var id int
		var content string
		var isVerified bool
		if err := rows.Scan(&id, &content, &isVerified); err != nil {
			return nil, err
		}
		notes = append(notes, &Note{ID: id, Content: content})
	}

	return notes, nil
}
