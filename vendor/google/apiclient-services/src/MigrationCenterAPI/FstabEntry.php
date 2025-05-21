<?php
/*
 * Copyright 2014 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */

namespace Google\Service\MigrationCenterAPI;

class FstabEntry extends \Google\Model
{
  /**
   * @var string
   */
  public $file;
  /**
   * @var int
   */
  public $freq;
  /**
   * @var string
   */
  public $mntops;
  /**
   * @var int
   */
  public $passno;
  /**
   * @var string
   */
  public $spec;
  /**
   * @var string
   */
  public $vfstype;

  /**
   * @param string
   */
  public function setFile($file)
  {
    $this->file = $file;
  }
  /**
   * @return string
   */
  public function getFile()
  {
    return $this->file;
  }
  /**
   * @param int
   */
  public function setFreq($freq)
  {
    $this->freq = $freq;
  }
  /**
   * @return int
   */
  public function getFreq()
  {
    return $this->freq;
  }
  /**
   * @param string
   */
  public function setMntops($mntops)
  {
    $this->mntops = $mntops;
  }
  /**
   * @return string
   */
  public function getMntops()
  {
    return $this->mntops;
  }
  /**
   * @param int
   */
  public function setPassno($passno)
  {
    $this->passno = $passno;
  }
  /**
   * @return int
   */
  public function getPassno()
  {
    return $this->passno;
  }
  /**
   * @param string
   */
  public function setSpec($spec)
  {
    $this->spec = $spec;
  }
  /**
   * @return string
   */
  public function getSpec()
  {
    return $this->spec;
  }
  /**
   * @param string
   */
  public function setVfstype($vfstype)
  {
    $this->vfstype = $vfstype;
  }
  /**
   * @return string
   */
  public function getVfstype()
  {
    return $this->vfstype;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(FstabEntry::class, 'Google_Service_MigrationCenterAPI_FstabEntry');
