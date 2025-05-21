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

namespace Google\Service\ChromeManagement;

class GoogleChromeManagementV1UsbPeripheralReport extends \Google\Collection
{
  protected $collection_key = 'categories';
  /**
   * @var string[]
   */
  public $categories;
  /**
   * @var int
   */
  public $classId;
  /**
   * @var string
   */
  public $firmwareVersion;
  /**
   * @var string
   */
  public $name;
  /**
   * @var int
   */
  public $pid;
  /**
   * @var int
   */
  public $subclassId;
  /**
   * @var string
   */
  public $vendor;
  /**
   * @var int
   */
  public $vid;

  /**
   * @param string[]
   */
  public function setCategories($categories)
  {
    $this->categories = $categories;
  }
  /**
   * @return string[]
   */
  public function getCategories()
  {
    return $this->categories;
  }
  /**
   * @param int
   */
  public function setClassId($classId)
  {
    $this->classId = $classId;
  }
  /**
   * @return int
   */
  public function getClassId()
  {
    return $this->classId;
  }
  /**
   * @param string
   */
  public function setFirmwareVersion($firmwareVersion)
  {
    $this->firmwareVersion = $firmwareVersion;
  }
  /**
   * @return string
   */
  public function getFirmwareVersion()
  {
    return $this->firmwareVersion;
  }
  /**
   * @param string
   */
  public function setName($name)
  {
    $this->name = $name;
  }
  /**
   * @return string
   */
  public function getName()
  {
    return $this->name;
  }
  /**
   * @param int
   */
  public function setPid($pid)
  {
    $this->pid = $pid;
  }
  /**
   * @return int
   */
  public function getPid()
  {
    return $this->pid;
  }
  /**
   * @param int
   */
  public function setSubclassId($subclassId)
  {
    $this->subclassId = $subclassId;
  }
  /**
   * @return int
   */
  public function getSubclassId()
  {
    return $this->subclassId;
  }
  /**
   * @param string
   */
  public function setVendor($vendor)
  {
    $this->vendor = $vendor;
  }
  /**
   * @return string
   */
  public function getVendor()
  {
    return $this->vendor;
  }
  /**
   * @param int
   */
  public function setVid($vid)
  {
    $this->vid = $vid;
  }
  /**
   * @return int
   */
  public function getVid()
  {
    return $this->vid;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleChromeManagementV1UsbPeripheralReport::class, 'Google_Service_ChromeManagement_GoogleChromeManagementV1UsbPeripheralReport');
