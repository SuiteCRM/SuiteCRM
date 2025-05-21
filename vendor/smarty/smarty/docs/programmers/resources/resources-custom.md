Custom Template Resources {#resources.custom}
=========================

You can retrieve templates using whatever possible source you can access
with PHP: databases, sockets, files, etc. You do this by writing
resource plugin functions and registering them with Smarty.

See [resource plugins](#plugins.resources) section for more information
on the functions you are supposed to provide.

> **Note**
>
> Note that you cannot override the built-in `file:` resource, but you
> can provide a resource that fetches templates from the file system in
> some other way by registering under another resource name.


    <?php

    /**
    * MySQL Resource
    *
    * Resource Implementation based on the Custom API to use
    * MySQL as the storage resource for Smarty's templates and configs.
    *
    * Table definition:
    * <pre>CREATE TABLE IF NOT EXISTS `templates` (
    *   `name` varchar(100) NOT NULL,
    *   `modified` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    *   `source` text,
    *   PRIMARY KEY (`name`)
    * ) ENGINE=InnoDB DEFAULT CHARSET=utf8;</pre>
    *
    * Demo data:
    * <pre>INSERT INTO `templates` (`name`, `modified`, `source`) VALUES ('test.tpl', "2010-12-25 22:00:00", '{$x="hello world"}{$x}');</pre>
    *
    * @package Resource-examples
    * @author Rodney Rehm
    */
    class Smarty_Resource_Mysql extends Smarty_Resource_Custom {
     // PDO instance
     protected $db;
     // prepared fetch() statement
     protected $fetch;
     // prepared fetchTimestamp() statement
     protected $mtime;

     public function __construct() {
         try {
             $this->db = new PDO("mysql:dbname=test;host=127.0.0.1", "smarty", "smarty");
         } catch (PDOException $e) {
             throw new SmartyException('Mysql Resource failed: ' . $e->getMessage());
         }
         $this->fetch = $this->db->prepare('SELECT modified, source FROM templates WHERE name = :name');
         $this->mtime = $this->db->prepare('SELECT modified FROM templates WHERE name = :name');
     }
     
     /**
      * Fetch a template and its modification time from database
      *
      * @param string $name template name
      * @param string $source template source
      * @param integer $mtime template modification timestamp (epoch)
      * @return void
      */
     protected function fetch($name, &$source, &$mtime)
     {
         $this->fetch->execute(array('name' => $name));
         $row = $this->fetch->fetch();
         $this->fetch->closeCursor();
         if ($row) {
             $source = $row['source'];
             $mtime = strtotime($row['modified']);
         } else {
             $source = null;
             $mtime = null;
         }
     }
     
     /**
      * Fetch a template's modification time from database
      *
      * @note implementing this method is optional. Only implement it if modification times can be accessed faster than loading the comple template source.
      * @param string $name template name
      * @return integer timestamp (epoch) the template was modified
      */
     protected function fetchTimestamp($name) {
         $this->mtime->execute(array('name' => $name));
         $mtime = $this->mtime->fetchColumn();
         $this->mtime->closeCursor();
         return strtotime($mtime);
     }
    }


    require_once 'libs/Smarty.class.php';
    $smarty = new Smarty();
    $smarty->registerResource('mysql', new Smarty_Resource_Mysql());

    // using resource from php script
    $smarty->display("mysql:index.tpl");
    ?>

      

And from within Smarty template:


    {include file='mysql:extras/navigation.tpl'}

      
