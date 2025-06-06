import os
import subprocess
import concurrent.futures


def format_file(file_path, formater_path, verbose=False):
    if verbose:
        print(f"  -> {file_path}")

    result = subprocess.run(
        [formater_path, "-i", "-style=file", file_path],
        capture_output=True,
        text=True,
    )
    return result.returncode == 0


def find_src_files(root_dir, exts, exclude_dirs):
    src_files = []
    for root, _, files in os.walk(root_dir):
        if any(ex_dir in root for ex_dir in exclude_dirs):
            continue

        for file in files:
            if any(file.endswith(ext) for ext in exts):
                src_files.append(os.path.join(root, file))
    return src_files


def load_ignore_config(root_dir):
    exclude_dirs = []
    ignore_file = os.path.join(root_dir, ".clang-format-ignore")
    if os.path.exists(ignore_file):
        with open(ignore_file, "r") as f:
            for line in f:
                line = line.strip()
                if not line or line.startswith("#"):
                    continue
                abs_path = os.path.abspath(os.path.join(root_dir, line))
                exclude_dirs.append(abs_path)
    return exclude_dirs


def main():
    root_dir = os.getcwd()
    exclude_dirs = load_ignore_config(root_dir)
    verbose = True
    threads = os.cpu_count()
    exts = [".cpp", ".h", ".hpp"]

    print("Preparing to format source files...")
    print(f"• Root directory: {root_dir}")
    print(f"• Excluded directories: {len(exclude_dirs)}")
    print(f"• Target file extensions: {', '.join(exts)}")
    print(f"• Using {threads} worker threads")
    print(f"• Verbose mode: {'enabled' if verbose else 'disabled'}")
    print("----------------------------------------")

    src_files = find_src_files(root_dir, exts, exclude_dirs)

    if src_files:
        success_count = 0
        with concurrent.futures.ThreadPoolExecutor(max_workers=threads) as executor:
            futures = [
                executor.submit(format_file, file_path, "clang-format", verbose)
                for file_path in src_files
            ]

            for future in concurrent.futures.as_completed(futures):
                if future.result():
                    success_count += 1

    print("----------------------------------------")
    print(f"fotmat done: {success_count}/{len(src_files)}")


if __name__ == "__main__":
    main()
