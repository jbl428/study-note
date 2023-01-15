package com.example.entity;

import com.example.entity.type.PostType;
import jakarta.persistence.*;
import jakarta.validation.constraints.NotNull;
import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;

@Entity
@NoArgsConstructor
@AllArgsConstructor
public class Post {
    @Id
    @GeneratedValue
    public Long id;

    @NotNull
    public String title;

    @NotNull
    public String content;

    @Enumerated(EnumType.STRING)
    @NotNull
    public PostType type;

    public static Post of(String title, String content, PostType type) {
        return new Post(null, title, content, type);
    }
}
