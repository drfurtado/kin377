import tkinter as tk
from tkinter import ttk, filedialog, messagebox
import json
import csv
from random import choice
from datetime import date

class RandomStudentSelector:
    def __init__(self, master):
        self.master = master
        self.master.title("Random Student Selector")
        self.master.geometry("800x600")

        self.students = []
        self.selected_student = None

        self.create_widgets()

    def create_widgets(self):
        # Buttons
        button_frame = ttk.Frame(self.master)
        button_frame.pack(pady=10)

        ttk.Button(button_frame, text="Upload CSV", command=self.upload_csv).pack(side=tk.LEFT, padx=5)
        ttk.Button(button_frame, text="Save Data", command=self.save_data).pack(side=tk.LEFT, padx=5)
        ttk.Button(button_frame, text="Load Data", command=self.load_data).pack(side=tk.LEFT, padx=5)
        ttk.Button(button_frame, text="Download CSV Template", command=self.download_csv_template).pack(side=tk.LEFT, padx=5)

        # Course Number Entry
        course_frame = ttk.Frame(self.master)
        course_frame.pack(pady=10)
        ttk.Label(course_frame, text="Course Number:").pack(side=tk.LEFT)
        self.course_entry = ttk.Entry(course_frame)
        self.course_entry.pack(side=tk.LEFT, padx=5)
        ttk.Button(course_frame, text="Select Random Student", command=self.select_random_student).pack(side=tk.LEFT)

        # Selected Student Frame
        self.selected_frame = ttk.Frame(self.master)
        self.selected_frame.pack(pady=10)

        # Student Table
        self.tree = ttk.Treeview(self.master, columns=("Name", "Course", "Last Participation", "Average Score"), show="headings")
        self.tree.heading("Name", text="Name")
        self.tree.heading("Course", text="Course")
        self.tree.heading("Last Participation", text="Last Participation")
        self.tree.heading("Average Score", text="Average Score")
        self.tree.pack(pady=10, fill=tk.BOTH, expand=True)

    def upload_csv(self):
        filename = filedialog.askopenfilename(filetypes=[("CSV Files", "*.csv")])
        if filename:
            with open(filename, 'r') as file:
                csv_reader = csv.reader(file)
                next(csv_reader)  # Skip header
                self.students = [{"name": row[0], "course": row[1], "participations": []} for row in csv_reader]
            self.update_table()

    def save_data(self):
        with open("student_data.json", "w") as file:
            json.dump(self.students, file)
        messagebox.showinfo("Success", "Data saved successfully!")

    def load_data(self):
        filename = filedialog.askopenfilename(filetypes=[("JSON Files", "*.json")])
        if filename:
            with open(filename, 'r') as file:
                self.students = json.load(file)
            self.update_table()

    def download_csv_template(self):
        filename = filedialog.asksaveasfilename(defaultextension=".csv", filetypes=[("CSV Files", "*.csv")])
        if filename:
            with open(filename, 'w', newline='') as file:
                writer = csv.writer(file)
                writer.writerow(["Name", "Course"])
                writer.writerow(["John Doe", "CS101"])
                writer.writerow(["Jane Smith", "CS102"])
            messagebox.showinfo("Success", f"CSV template saved as {filename}")

    def select_random_student(self):
        if self.students:
            self.selected_student = choice(self.students)
            self.update_selected_frame()

    def update_selected_frame(self):
        for widget in self.selected_frame.winfo_children():
            widget.destroy()

        if self.selected_student:
            ttk.Label(self.selected_frame, text=f"Selected Student: {self.selected_student['name']} (Course: {self.selected_student['course']})").pack()
            ttk.Button(self.selected_frame, text="Attempted", command=lambda: self.handle_participation(1)).pack(side=tk.LEFT, padx=5)
            ttk.Button(self.selected_frame, text="Partial", command=lambda: self.handle_participation(2)).pack(side=tk.LEFT, padx=5)
            ttk.Button(self.selected_frame, text="Complete", command=lambda: self.handle_participation(3)).pack(side=tk.LEFT, padx=5)

    def handle_participation(self, score):
        today = date.today().isoformat()
        self.selected_student['participations'].append({"date": today, "score": score, "course": self.course_entry.get()})
        self.selected_student = None
        self.update_selected_frame()
        self.update_table()

    def update_table(self):
        for i in self.tree.get_children():
            self.tree.delete(i)

        for student in self.students:
            last_participation = student['participations'][-1]['date'] if student['participations'] else 'N/A'
            avg_score = sum(p['score'] for p in student['participations']) / len(student['participations']) if student['participations'] else 'N/A'
            avg_score = f"{avg_score:.2f}" if isinstance(avg_score, float) else avg_score
            self.tree.insert("", "end", values=(student['name'], student['course'], last_participation, avg_score))

if __name__ == "__main__":
    root = tk.Tk()
    app = RandomStudentSelector(root)
    root.mainloop()
    
