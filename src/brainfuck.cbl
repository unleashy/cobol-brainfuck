        >>source format is free
identification division.
program-id. brainfuck.

environment division.
configuration section.
repository.
    function char trim ord intrinsic.

input-output section.
file-control.
    select Source-File assign to dynamic file-arg
        file status is file-status.

data division.
file section.
    fd Source-File.
    01 fs-source-file.
        02 fs-instruction  pic X.

working-storage section.
    01 tmp-input       pic X.
    01 file-arg        pic X(255) value is space.
    01 file-status     pic 99.
    01 source-len      pic 999 value is zero.
    01 brainfuck.
        02 brainfuck-counter  usage is binary-int.
        02 brainfuck-tape     usage is binary-char unsigned
                              occurs 30000 times indexed by brainfuck-dptr.
        02 brainfuck-code     pic X
                              occurs 0 to 16384 times depending on source-len
                              indexed by brainfuck-iptr.

procedure division.
declaratives.

file-error section.
use after standard error procedure on Source-File.
    evaluate file-status
        when 35    display "cannot find file " trim(file-arg, trailing) upon stderr
        when other display "error with file (" file-status ")" upon stderr
    end-evaluate.

    goback.
end declaratives.

main section.
main-procedure.
    display 1 upon argument-number.
    accept file-arg from argument-value
        on exception perform no-such-arg.

    perform bf-read.
    perform bf-run.

    stop run.

bf-read.
    open input Source-File.

    perform forever
        read Source-File next record
            at end exit perform
        end-read

        *> only add to brainfuck-code the valid brainfuck instructions
        evaluate fs-instruction
            when '>'
            when '<'
            when '+'
            when '-'
            when '.'
            when ','
            when '['
            when ']'
                add 1 to source-len
                move fs-instruction to brainfuck-code(brainfuck-iptr)
                set brainfuck-iptr up by 1
        end-evaluate
    end-perform.

    close Source-File.

bf-run.
    set brainfuck-iptr to 1.

    perform until brainfuck-iptr > source-len
        move 1 to brainfuck-counter

        evaluate brainfuck-code(brainfuck-iptr)
            when '>' set brainfuck-dptr up   by 1
            when '<' set brainfuck-dptr down by 1
            when '+' add      1 to   brainfuck-tape(brainfuck-dptr)
            when '-' subtract 1 from brainfuck-tape(brainfuck-dptr)
            when '.' display char(brainfuck-tape(brainfuck-dptr) + 1) with no advancing
            when ',' perform bf-input
            when '[' perform bf-rbracket
            when ']' perform bf-lbracket
        end-evaluate

        set brainfuck-iptr up by 1
    end-perform.

bf-input.
    accept tmp-input.
    move ord(tmp-input) to brainfuck-tape(brainfuck-dptr).

bf-rbracket.
    if brainfuck-tape(brainfuck-dptr) is zero
        set brainfuck-iptr up by 1

        perform until brainfuck-counter <= 0

            if brainfuck-iptr > source-len
                perform unbalanced-brackets
            end-if

            evaluate brainfuck-code(brainfuck-iptr)
                when '[' add      1 to   brainfuck-counter
                when ']' subtract 1 from brainfuck-counter
            end-evaluate

            set brainfuck-iptr up by 1
        end-perform

        set brainfuck-iptr down by 1
    end-if.

bf-lbracket.
    if brainfuck-tape(brainfuck-dptr) is not zero
        set brainfuck-iptr down by 1

        perform until brainfuck-counter <= 0

            if brainfuck-iptr < 0
                perform unbalanced-brackets
            end-if

            evaluate brainfuck-code(brainfuck-iptr)
                when ']' add      1 to   brainfuck-counter
                when '[' subtract 1 from brainfuck-counter
            end-evaluate

            set brainfuck-iptr down by 1
        end-perform

        set brainfuck-iptr up by 1
    end-if.

unbalanced-brackets.
    display "unbalanced brackets." upon stderr.
    stop run returning 1.

no-such-arg.
    display "missing 'filename' argument" upon stderr.
    stop run returning 1.

end program brainfuck.
