# Security and data protection

## Reporting a vulnerability

Please do not open a public issue for a security problem. Write to
mgutierrezr_9902@outlook.com with a description and, if possible, steps to
reproduce it. You will receive a reply as soon as possible.

## Patient data

RegiStat is designed to work offline, and nothing in the package sends data
over the network. Even so:

* **Never post patient data in issues, pull requests or discussions.** Use the
  synthetic generator (`synthetic_t2d()`) to build a reproducible example.
* Use a pseudonymised study code as the record identifier, never the hospital
  file number, name, initials or date of birth.
* Keep real databases outside the repository folder. The `.gitignore` blocks
  common database and spreadsheet formats as a safety net, not as a guarantee.
* You are responsible for complying with the ethics approvals, institutional
  policies and data protection laws that apply to your study.

RegiStat is research software. It is not a medical device and has not been
validated for regulatory compliance (for example, 21 CFR Part 11).
