class { "java":
  distribution => 'jre',
}

package { "nodejs":
  ensure => 'installed',
}

package { "clang":
  ensure => 'installed',
}

package { "nasm":
  ensure => 'installed',
}

package { "gcc-multilib":
  ensure => 'installed',
}

