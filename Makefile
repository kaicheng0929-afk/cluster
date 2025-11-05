R := Rscript
TASK1 := Task1.R
TASK2 := Task2.R
RESULTS_DIR := results
FIG_DIR := figures

all: task1 task2

task1:
	@echo "▶ Running Task1..."
	$(R) $(TASK1)

task2:
	@echo "▶ Running Task2..."
	$(R) $(TASK2)

clean:
	rm -rf $(RESULTS_DIR) $(FIG_DIR)
	@echo "🧹 Cleaned all generated results."

help:
	@echo "Usage:"
	@echo "  make        # Run both Task1 and Task2"
	@echo "  make task1  # Run only Task1"
	@echo "  make task2  # Run only Task2"
	@echo "  make clean  # Remove generated results"
