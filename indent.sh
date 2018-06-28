tmp="fdoadfaofdhpaifnapodfeaia@0eth40qh@gf"
for file in *.ml; do
    (ocp-indent $file) > $tmp
    cp $tmp $file
done

rm $tmp