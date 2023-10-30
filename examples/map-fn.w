a = nsert "hello" 56 emptyMap
nsertTwo = λ k . λ l . λ v . λ map . nsert l v (nsert k v map)
b = nsertTwo "s" "b" 5555 a
c = nsertTwo 1 2 "hello" emptyMap
