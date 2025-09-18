#!/bin/bash
echo "Setting up custom merge drivers for CSV and RDA files..."

# Create the merge driver scripts in .git directory
mkdir -p .git

# Create CSV merge driver
cat > .git/csv-merge-driver.sh << 'CSVEOF'
#!/bin/bash
CURRENT=$1
OTHER=$3
PATHNAME=$5

echo "Auto-merging CSV file: $PATHNAME"

# Create temporary files
TEMP_DIR=$(mktemp -d)
MERGED="${TEMP_DIR}/merged.csv"

# Get header from current version
head -n1 "$CURRENT" > "$MERGED"

# Combine all unique rows from both files (excluding headers)
{
    tail -n+2 "$CURRENT"
    tail -n+2 "$OTHER"
} | sort -u >> "$MERGED"

# Replace current file with merged result
cp "$MERGED" "$CURRENT"
rm -rf "$TEMP_DIR"

echo "Successfully merged $PATHNAME"
exit 0
CSVEOF

# Create RDA merge driver
cat > .git/rda-merge-driver.sh << 'RDAEOF'
#!/bin/bash
CURRENT=$1
PATHNAME=$5

echo "Regenerating $PATHNAME using existing R scripts..."

if [[ "$PATHNAME" == *"pgsources.rda" ]]; then
    R_SCRIPT_PATH="data_raw/pgsources.R"
elif [[ "$PATHNAME" == *"pgvariables.rda" ]]; then
    R_SCRIPT_PATH="data_raw/pgvariables.R"
else
    echo "Unknown RDA file: $PATHNAME"
    exit 1
fi

if [[ ! -f "$R_SCRIPT_PATH" ]]; then
    echo "Error: R script $R_SCRIPT_PATH not found"
    exit 1
fi

if command -v Rscript >/dev/null 2>&1; then
    echo "Running: Rscript $R_SCRIPT_PATH"
    Rscript "$R_SCRIPT_PATH"
    R_EXIT_CODE=$?
    
    if [[ $R_EXIT_CODE -eq 0 ]]; then
        echo "Successfully regenerated $PATHNAME"
    else
        echo "Error: R script failed"
        exit 1
    fi
else
    echo "Error: Rscript not found. Please install R."
    exit 1
fi

exit 0
RDAEOF

# Make scripts executable
chmod +x .git/csv-merge-driver.sh
chmod +x .git/rda-merge-driver.sh

# Configure git to use these merge drivers
git config merge.csv-union.name "CSV union merge that preserves all additions"
git config merge.csv-union.driver ".git/csv-merge-driver.sh %A %O %B %L %P"

git config merge.rda-regenerate.name "RDA regeneration from existing R scripts"
git config merge.rda-regenerate.driver ".git/rda-merge-driver.sh %A %O %B %L %P"

echo "✓ Created merge driver scripts"
echo "✓ Configured git merge drivers"

# Create or update .gitattributes
if [[ ! -f .gitattributes ]]; then
    cat > .gitattributes << 'ATTREOF'
# Auto-merge CSV files by combining all additions
data_raw/sources.csv merge=csv-union
data_raw/variables.csv merge=csv-union

# Auto-regenerate RDA files from R scripts
data/pgsources.rda binary merge=rda-regenerate
data/pgvariables.rda binary merge=rda-regenerate
ATTREOF
    echo "✓ Created .gitattributes file"
else
    echo "⚠ .gitattributes already exists - you'll need to add the merge configurations manually"
    echo "Add these lines to your .gitattributes file:"
    echo "data_raw/sources.csv merge=csv-union"
    echo "data_raw/variables.csv merge=csv-union" 
    echo "data/pgsources.rda binary merge=rda-regenerate"
    echo "data/pgvariables.rda binary merge=rda-regenerate"
fi

echo ""
echo "🎉 Setup complete!"
echo ""
echo "Next steps:"
echo "1. Run: git add .gitattributes"
echo "2. Run: git commit -m 'Add automatic merge drivers for CSV and RDA files'"
echo "3. Test with a merge that has conflicts"
