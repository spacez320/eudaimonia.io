---
layout: post
title: Zero-knowledge Password Management
deckhead: A guide to simple and effective password hygiene.
---

Passwords can be a sensitive, confusing topic. They're like opinions --
everyone's got one, and specifically opinions on how passwords should be
managed. Here I try to outline the reasoning around how I think about
passwords, and how to easily avoid bad habits.

## Presenting the problem

You may have noticed that what we're told about passwords is inconsistent.
While your banking website declares that the only secure password is one that
has twelve letters, at least three capitals, a math equation, and a prime
number, your phone (which might have your bank's app on it) only wants you to
move your thumb in a certain direction to unlock it. Ivory towers hang banners
that say we need give up and find a way to get to the post-password world [^1],
but for most people on the ground that doesn't help much. 

Despite the confusion, it's a pretty common message that passwords are one of
those things that most people should better manage [^8], but it's really hard
to even know what "better" means, or what the risk really is. Meanwhile, many
of us commit cardinal sins like using the same password for everything, or
using a password that's simple because it's easy to remember, or using a
password that has your birthday in it -- all of which are clearly wrong.

As it turns out, doing the right thing with your password management really
boils down to a very clear set of rules.

<ol style="text-align:center">
<li>Make your password long.</li>
<li>Never use the same password twice.</li>
</ol>

That's it.

Following these two rules set you up with the best defense against common
problems, including password leaks, bad password protection on the part of a
company holding your data, and malicious password guessing.

It's not as important that your password have strange characters, at least one
capital letter and number, or that they don't contain strings like '123'. They
just need to be different, and they need to be lengthy.

## Why worry?

So what do these two rules actually solve?

At the end of the day, a password is a secret shared between you and someone
else that should stop anyone but you from interacting with them. The most
common problems appear when someone guesses your password, or learns about it
directly from the other party [^2] [^3]. Both of these events happen all the
time; malicious people on the internet write software designed to rapidly guess
passwords and major password leaks make the news every few months or so. In
fact, any sufficiently good password strategy should both attempt to prevent
guessing, but also assume that your password will eventually be guessed or
leaked.

Explaining the rules' role in this strategy, like the rules themselves, is
thankfully quite simple as well.

## Step one: make your password long

Making your password long is the only real requirement to make it hard to
guess. Period. There's a very mathematical way to explain this, but it's
possible to distil it down into something straight-forward.

If I am trying to guess your password, I don't want to do it manually, I can
easily write some program that tries passwords until something goes through. So
the amount of effort I have to put in to do this directly correlates with how
complex your password is, or the entropy. A password with high entropy should
be difficult to guess, and the easiest and most effective way to add entropy is
to add length [^4].

For example, let's say that my password is all lower-case alphanumeric
characters, something like `azbycx` -- and we want to see if that's guessable.
A human being sitting down and trying to brute force this might start with `a`,
and then maybe `b`, and then perhaps `c`, and eventually they'll get to `z` and
have to get creative and try something like `aa`. And so on forever. A human
being doing this would take a very long time, of course, but a common computer
(that can run several million operations a second) running known password
cracking software could probably guess `azbycx` in about *10 milliseconds*
using this strategy.

> Most websites will introduce limits that artificially slow down password
guessing and prevent this type of attack, but it's still the case that the
complexity of your password directly correlates with the time and energy needed
to figure it out.

So what happens if we make it longer? If we add three more characters, this
increases to *two minutes*; a nearly 10000x order of magnitude increase, but
still not very good.

However, luckily for us, it turns out a linear increase in size adds an
exponential amount of work to guess, so we don't have that
much farther to go.

| Password        | Size  | Time to guess      |
| --------------- | ----: | ------------------ |
| azb             | 3     | 400 nanoseconds    |
| azbycx          | 6     | 8 miliseconds      |
| azbycxdwe       | 9     | 2 minutes          |
| azbycxdwevfu    | 12    | 4 weeks            |
| azbycxdwevfugth | 15    | one thousand years |

Somewhere between four weeks and one thousand years is probably a good place to
start, and we didn't need to even use a `!@#$%&` or capital letters or numbers
or any of that jazz.

> The astute of you may have noticed that I'm following a pattern --
interchanging ascending and descending characters in the English alphabet.
This isn't the best idea, as someone could guess at this pattern and narrow the
scope of search. In much the same way, it's bad to use any pattern like '12345'
or arguably even dictionary words. Better would be to use some random selection
of characters, event if some repeat.

To be clear, adding extra characters or capital letters is not a *bad* idea, it
ultimately makes your password more difficult to guess because it increases the
number of characters a guesser has to consider. However, length is ultimately
more important and impactful.

## Step two: never use the same password twice

So, I'm guessing you stopped reading at the end of this section and changed
your go-to password to be something nice and long. Good for you! Despite your
best efforts though, tomorrow's news will mention that your email service just
leaked all their users' passwords to the internet. Frantically, you'll Google
your password (which, by the way; don't do that) and sure enough -- it shows up
in some list on some shady website. Sad times.

The problem is that you're using that password everywhere. *Everywhere.*
Probably on websites you haven't been to in years. That password has been like
a life-long friend, and your fingers know how to type it effortlessly.

It's probably pretty clear why using different passwords is a good idea -- if
(and when) your password becomes public knowledge, the impact on you is limited
to just the service you're using it for, and the cleanup becomes much easier.
In addition, if someone who has your password can assume that you use it
elsewhere (like your email), this means they can't compromise more of your
things, so the overall effect is also much less than it could have been.

## Putting everything into practice

The two rules, despite their simplicity, do lead to a complex problem --
password management. Using just your head and your hands, making long passwords
that are different in each place is really hard to do, since at the end of the
day you have to remember what they are. For me personally, if I try to come up
with a good password for some new login, despite all my efforts at mnemonics
and tricks, I'm always force to click the "Lost Password" link the next time I
find myself there.

Ideally, we could use some helper service that would store our passwords for us
in some secure way. That way, we can just pull out passwords that we want to
use just when we actually need them. This is where the concept of "zero
knowledge" passwords comes from -- you don't actually have to remember the
password for Facebook, or Google, or Twitter, or anything, just have your
password management tool generate it and then copy-paste. For almost every
service that I use on the internet, I have absolutely no idea what the password
is off the top of my head, and for most I haven't even seen what it is.

A long time ago, for some strange reason, I tried to create my own password
management system using GPG and the command line, which ended up being a set of
shell scripts to manage a file of password entries as a very simple key/value
store. This resulted in [my GitHub project, **p**](https://github.com/spacez320/p),
which I used for about a year before discovering something more community
driven that filled the same role and did everything better: [**pass**](https://www.passwordstore.org/).

## Using pass

**pass** is, at its heart, a CLI tool that makes it very easy to keep all my
passwords with me, and add/change/retrieve all my passwords at any time.

Here's some examples of using it:

- Create a really long (32 characters) new password for Facebook.  
  ```
  pass generate accounts/facebook 32
  ```

- Search to see if I already have a password for AirBnB.  
  ```
  pass search airbnb
  ```

- Change a password because I needed to rotate it, or it didn't fit the
  requirements of the website.  
  ```
  pass edit accounts/mybank
  ```

With this system in place, I never have to think that hard about passwords, but
I still follow the two rules that keep me in a good place. I can produce
passwords with a length that makes me reasonably sure people can't just guess
them, and if someone does figure out one of my passwords, it is very easy to
change.

## Reaching password utopia

Over time I wanted to do more with **pass** -- to make it totally flexible for
my needs, and to make sure I could use it on any computer that I regularly find
myself on. I also wanted to make sure my passwords were safe from accidental
loss, theft, and etc.

Here's the setup I use now.

- On a set of encrypted thumb-drives (using standard LUKS) [^5], I store a copy
  of my **pass** database.

- Each time I log into my computer, a shell script triggered by a login 
  (`.bashrc`) looks for those drives, and if it it finds one mounted, sets a
  special environment variable with its path.

      priv() {
        for mount in /mnt/priv1 /mnt/priv2 /mnt/priv3; do
          if mountpoint $mount &> /dev/null; then
            echo $mount
            break
          fi
        done
      }

      export PRIV=$(priv)

- The **pass** program can use this exported value to look for passwords in a
  specific place.

      export PASSWORD_STORE_DIR=$PRIV/.pass

- I freely add and manipulate passwords on whatever drive I happen to be using.
  In order to keep things in sync, I take advantage of the fact that **pass**
  uses Git behind the scenes. In fact, you can freely manipulate this directly
  with Git tools with the **pass** CLI tool itself.

      pass git status

  On a **private** (Private!!! Even though **pass** uses GPG encryption, you
  still don't want to expose the password files to the public!) remote Git
  repository, I store a master copy of my passwords and push/pull changes to
  and from whatever drive I happen to be working on.

      # Add that remote repository, if it's not already there.
      pass git remote add origin <some private Git repository>

      # Get passwords that I've changed elsewhere.
      pass git pull origin master

      # Share my changes for future me.
      pass git push origin master

Having these drives means I can take my passwords anywhere, that I don't have
to leave them on every computer I use, and encrypting them means I shouldn't
have to freak out if I happen to lose one. All the while I'm still following
the golden password rules.

## Additional thoughts

You definitely don't need to go down the path of using **pass**, there are
other programs out there that do things like integrate with your browser, or
are easier to install and use.

If you ever want to check how sturdy the password you're using for things is, I
recommend [howsecureismypassword.net](https://howsecureismypassword.net).

My encryption of thumb drives is arguably better handled by the
[YubiKey project](https://www.yubico.com/).

Keep in mind, this article just talks about the *basic rules of making
passwords* and highlights a single password manager, but perfect security is a
never-ending struggle. There are nuances to passwords that are important to
think about and other things, like multi-factor authentication, that compliment
good password usage.  _Schneier on Security_ has some articles which dig more
into the topic and talk about what comes next [^6] [^7].

I definitely don't want to be spreading bad advice, especially in the area of
computer security. If you feel like I've said something misleading, please drop
me a line so I can make amends.

[^1]: Tsukayama, Hayley. "Microsoft is trying to kill passwords. It can't happen soon enough." <http://www.latimes.com/business/technology/la-fi-tn-password-microsoft-20180211-story.html>, 2018.
[^2]: Albright, Dann. "Password Leaks Are Happening Now: Here’s How to Protect Yourself." <https://www.makeuseof.com/tag/passwords-leaks-happening-time-heres-protect/>, 2016.
[^3]: "OWASP Top 10 - 2017." <https://www.owasp.org/images/7/72/OWASP_Top_10-2017_%28en%29.pdf.pdf>, 2017.
[^4]: Wikipedia. "Password strength." <https://en.wikipedia.org/wiki/Password_strength>.
[^5]: Wallen, Jack. "Easily Encrypt your Flash Drives with Linux." <https://www.linux.com/learn/easily-encrypt-your-flash-drives-linux>, 2016.
[^6]: Schneier, Bruce. "Password Advice." <https://www.schneier.com/blog/archives/2009/08/password_advice.html>, 2009.
[^7]: Schneier, Bruce. "Changes in Password Best Practices." <https://www.schneier.com/blog/archives/2017/10/changes_in_pass.html>, 2017.
[^8]: Walters, Richard. "Insecure Passwords or Insecure People?" <"https://www.infosecurity-magazine.com/opinions/insecure-passwords-insecure-people/>, 2015.
