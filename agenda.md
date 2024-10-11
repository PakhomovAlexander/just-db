







Today we are going to:
- [x] SELECT, FROM, 

- [ ] WHERE statement support

select c1, c2 from t where c1 = 1 and c2 = 2;

        Node(type = SELECT) 
      /                   \
    select_col_list     Node(type = WHERE)   from(t)
       (c1, c2)
                        /               \
                    Node(type = AND)   Node(type = AND)
                    /           \       /           \
                Node(c1=1)  Node(c2=2) Node(c1=1)  Node(c2=2)









