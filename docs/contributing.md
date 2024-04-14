<!-- omit from toc -->
# Contributing

Comments and issues are welcome [here](https://github.com/LionelDraghi/ArchiCheck/issues/new).  
Feel free to suggest whatever improvement.
I'm not a native english speaker. English improvements are welcome, in documentation as in the code.  
Use cases or features suggestions are also welcome.  

## submitting code / docs

To propose some patch          | git command
-------------------------------|-------------------------------------
 1. Fork the project           | fork button on https://github.com/LionelDraghi/ArchiCheck
 2. Clone your own copy        | `git clone https://github.com/your_user_name/ArchiCheck.git`
 3. Create your feature branch | `git checkout -b my-new-feature`    
 4. Commit your changes        | `git commit -am 'Add some feature'` 
 5. Push to the branch         | `git push origin my-new-feature`    
 6. Create new Pull Request    | on your GitHub fork, go to "Compare & pull request".

> [!CAUTION] 
> Coverage is in transition phase from `lcov` to `gnat coverage`, in the mean time, following recommendations do no apply

> The project policy is that code shall be 100% covered (except debug or error specific lines).
> Coverage is computed on each `make check`, and non covered code is easy to check with (for example) `chromium docs/lcov/index.html`.  
> **NB : please note that code proposed with matching tests, doc and complete coverage is very appreciated!**  
