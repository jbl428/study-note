package com.example.entity.type;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum PostType {
    QUESTION("질문"),
    ANSWER("답변");

    private final String name;
}
