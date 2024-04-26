import tkinter as tk
from tkinter import messagebox

# Define the test examples
recall_examples = [
    "PT: Have a patient demonstrate the proper form for a rehabilitation exercise they were taught, such as a seated knee extension, without any cues.",
    "Sports: Ask a basketball player to perform a specific dribbling drill they learned in practice, such as a figure-eight pattern, from memory.",
    "Dance: Require a dancer to perform a short choreographed sequence they were taught in a previous lesson without the aid of music or demonstration.",
    "PT: Ask a patient to explain the steps for safely transferring from a wheelchair to a bed, which they were previously instructed on.",
    "Sports: Have a soccer player demonstrate the proper technique for a throw-in without any prompts or reminders."
]

recognition_examples = [
    "PT: Present a patient with several variations of a balance exercise and ask them to identify the one that was taught to them as most appropriate for their condition.",
    "Sports: Show a tennis player video clips of various serve techniques and have them select the one that best matches what they were instructed to use.",
    "Dance: Demonstrate two similar dance moves and ask the dancer to choose the one that was part of the choreography they recently learned.",
    "PT: Provide a patient with a list of activities and have them identify which ones are safe for them to perform based on their previous education.",
    "Sports: Display images of different golf club grips and ask a golfer to select the one their coach recommended for improved swing consistency."
]

# Initialize the app window
window = tk.Tk()
window.title("Recall vs Recognition Test")

# Set the font size to 50pt
font_size = 50
font = ("Arial", font_size)

# Function to check the answer and display the result
def check_answer():
    selected_option = var.get()
    if (selected_option == 1 and current_example in recall_examples) or (selected_option == 2 and current_example in recognition_examples):
        messagebox.showinfo("Result", "Correct!", parent=window)
    else:
        messagebox.showinfo("Result", "Incorrect. Please try again.", parent=window)

# Function to display the next example
def next_example():
    global current_example
    if examples:
        current_example = examples.pop(0)
        example_label.config(text=current_example)
    else:
        messagebox.showinfo("Complete", "All examples have been shown.", parent=window)
        window.destroy()

# Combine examples and shuffle them
examples = recall_examples + recognition_examples
import random
random.shuffle(examples)

# Display the first example
current_example = examples.pop(0)
example_label = tk.Label(window, text=current_example, wraplength=800, font=font)
example_label.pack(pady=20)

# Create radio buttons for selecting the answer
var = tk.IntVar()
recall_radio = tk.Radiobutton(window, text="Recall Test", variable=var, value=1, font=font)
recall_radio.pack()
recognition_radio = tk.Radiobutton(window, text="Recognition Test", variable=var, value=2, font=font)
recognition_radio.pack()

# Create buttons for checking the answer and moving to the next example
check_button = tk.Button(window, text="Check Answer", command=check_answer, font=font)
check_button.pack(pady=10)
next_button = tk.Button(window, text="Next Example", command=next_example, font=font)
next_button.pack()

# Start the app
window.mainloop()