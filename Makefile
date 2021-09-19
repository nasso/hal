##
## EPITECH PROJECT, 2021
## hal
## File description:
## Makefile to build the project
##

NAME = hal

all: $(NAME)

clean:
	stack clean

fclean: clean
	$(RM) $(NAME)

re: fclean all

tests_run:
	stack test

$(NAME):
	stack install $(NAME)

.PHONY: all clean fclean re
