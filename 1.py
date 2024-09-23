from tkinter import*
import hashlib
from tkinter.messagebox import showerror, showinfo

root = Tk()
root.title('LAB2')
root.geometry('300x300')
root['bg'] = '#d5e8e8'
root.resizable(False, False)

label = Label(text = "Введите пароль")
label.pack()

def create_password():
    global password
    password = hashlib.sha256((entry1.get()+''.join('0' if char in 'aeiouAEIOU' else '1' if char.isalpha() else char for char in entry1.get())).encode())
    #print(password.hexdigest())

def check_password():
    if password.hexdigest() == hashlib.sha256((entry2.get()+''.join('0' if char in 'aeiouAEIOU' else '1' if char.isalpha() else char for char in entry2.get())).encode()).hexdigest():
        showinfo("Ура", "Верный пароль")
    else:
        showerror("Упс", "Неверный пароль")


entry1 = Entry(root, width = 15, font = ('', 15))
entry1.place(x = 15, y = 50)

create_button = Button(root, text="Создать", width = 9, command = create_password)
create_button.place(y = 50, x = 210)

entry2 = Entry(root, width = 15, font = ('', 15))
entry2.place(x = 15, y = 100)

check_button = Button(root, text="Проверить", width = 9, command = check_password)
check_button.place(y = 100, x = 210)


root.mainloop()