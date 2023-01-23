package com.example.entity;

import com.example.entity.type.PostType;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

@Entity
@NoArgsConstructor
@AllArgsConstructor
public class Post {
    @Id
    @GeneratedValue
    @NotNull
    public Long id;

    @NotNull
    public String title;

    @Nullable
    public String content;

    @Enumerated(EnumType.STRING)
    @NotNull
    public PostType type;

    @NotNull
    public static Post of(@NotNull String title, @NotNull String content, @NotNull PostType type) {
        return new Post(null, title, content, type);
    }
}
