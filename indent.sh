for file in *.ml; do
    (ocp-indent $file) > tmp
    cp tmp $file
done

rm tmp
