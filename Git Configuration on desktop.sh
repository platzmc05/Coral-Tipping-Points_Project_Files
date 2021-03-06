# Configure git

# This line may be needed to change the default branch name from master to main.
git config --global init.defaultBranch main 

# Replace the ... in these lines with the user name and email address
# you have already set up on GitHub.com.
git config --global user.name platzmc05
git config --global user.email mplatz@usf.edu

# Link your local repository to the origin repository on GitHub, by
# copying the code shown on your GitHub repo under the heading:
# "…or push an existing repository from the command line"

git remote add origin https://github.com/platzmc05/handouts.git
git push -u origin main
