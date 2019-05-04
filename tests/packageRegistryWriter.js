const fs = require('fs')

const writeInt = (num) => {
  const buffer = Buffer.alloc(1)
  buffer.writeInt8(num)
  
  return buffer;
}

const writeLong = (num) => {
  const buffer = Buffer.alloc(8)
  let cursor = buffer.writeInt32BE(0)
  buffer.writeInt32BE(num, cursor)

  return buffer
}

const writeString = (str) => {
  const buffer = Buffer.alloc(str.length + 8)
  let cursor = buffer.writeInt32BE(0)
  cursor = buffer.writeInt32BE(str.length, cursor)
  buffer.write(str, cursor)

  return buffer
}

const writePackageRegistry = async (filename, registry) => {
  var wstream = fs.createWriteStream(filename);
  await wstream.write(writeLong(0));
  await wstream.write(writeLong(registry.length));
  for (var i = 0; i < registry.length; i++) {
    const entry = registry[i]
    await wstream.write(writeString(entry.name.author))
    await wstream.write(writeString(entry.name.project))
    await wstream.write(writeLong(entry.versions.length))
    for (var v = 0; v < entry.versions.length; v++) {
      const version = entry.versions[v]
      await wstream.write(writeInt(version.major))
      await wstream.write(writeInt(version.minor))
      await wstream.write(writeInt(version.patch))
    }
  }  
  await wstream.end();  
}

const version = (major, minor, patch) => {
  return {
    major,
    minor,
    patch
  }
}

const entry = (author, project, versions) => {
  return {
    name: {
      author,
      project
    },
    versions
  }
}

const registry = [
  entry("elm-explorations", "elmer", [ version(6, 0, 0) ])
]

if (process.argv.length != 3) {
  console.log("node ./packageRegistryWriter.js <filename>")
  process.exit()
}

const filename = process.argv[2]

console.log(`Writing package registry to ${filename} ...`)
writePackageRegistry(filename, registry).then(() => {
  console.log("Done!")
})
