# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright (C) 2026 buddhilw
#
# Relay lab node for grammar's ssh tests (GRAMMAR_SSHD=1). One image, run
# as several containers on a private network: user `relay` (login /bin/sh,
# passwordless sudo), user `cshu` (login /bin/tcsh), user `fishu` (login
# /usr/bin/fish), user `app` (sudo target). The test copies one temporary
# public key in at start; nothing persists.
#
# The lab settings go in sshd_config.d, which the stock config includes
# BEFORE its own lines: for sshd the first value of a keyword wins.
FROM alpine:3.19
RUN apk add --no-cache openssh-server openssh-client sudo python3 tcsh fish \
 && ssh-keygen -A \
 && for u in relay cshu fishu app; do adduser -D "$u"; done \
 && sed -i 's#^\(cshu:.*:\)[^:]*$#\1/bin/tcsh#; s#^\(fishu:.*:\)[^:]*$#\1/usr/bin/fish#' /etc/passwd \
 && sed -i 's#^\(relay\|cshu\|fishu\|app\):!:#\1:*:#' /etc/shadow \
 && echo 'relay ALL=(ALL) NOPASSWD: ALL' > /etc/sudoers.d/relay \
 && mkdir -p /etc/ssh/keys /etc/ssh/sshd_config.d \
 && printf '%s\n' 'PasswordAuthentication no' 'KbdInteractiveAuthentication no' \
      'PermitRootLogin no' 'AllowTcpForwarding yes' 'AuthorizedKeysFile /etc/ssh/keys/%u' \
      > /etc/ssh/sshd_config.d/lab.conf
EXPOSE 22
CMD ["/usr/sbin/sshd", "-D", "-e"]
