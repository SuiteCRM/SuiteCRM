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

namespace Google\Service\NetAppFiles;

class MountOption extends \Google\Model
{
  /**
   * @var string
   */
  public $export;
  /**
   * @var string
   */
  public $exportFull;
  /**
   * @var string
   */
  public $instructions;
  /**
   * @var string
   */
  public $ipAddress;
  /**
   * @var string
   */
  public $protocol;

  /**
   * @param string
   */
  public function setExport($export)
  {
    $this->export = $export;
  }
  /**
   * @return string
   */
  public function getExport()
  {
    return $this->export;
  }
  /**
   * @param string
   */
  public function setExportFull($exportFull)
  {
    $this->exportFull = $exportFull;
  }
  /**
   * @return string
   */
  public function getExportFull()
  {
    return $this->exportFull;
  }
  /**
   * @param string
   */
  public function setInstructions($instructions)
  {
    $this->instructions = $instructions;
  }
  /**
   * @return string
   */
  public function getInstructions()
  {
    return $this->instructions;
  }
  /**
   * @param string
   */
  public function setIpAddress($ipAddress)
  {
    $this->ipAddress = $ipAddress;
  }
  /**
   * @return string
   */
  public function getIpAddress()
  {
    return $this->ipAddress;
  }
  /**
   * @param string
   */
  public function setProtocol($protocol)
  {
    $this->protocol = $protocol;
  }
  /**
   * @return string
   */
  public function getProtocol()
  {
    return $this->protocol;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(MountOption::class, 'Google_Service_NetAppFiles_MountOption');
