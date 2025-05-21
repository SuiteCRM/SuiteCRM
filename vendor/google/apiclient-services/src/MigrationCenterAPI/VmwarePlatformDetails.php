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

class VmwarePlatformDetails extends \Google\Model
{
  /**
   * @var string
   */
  public $esxHyperthreading;
  /**
   * @var string
   */
  public $esxVersion;
  /**
   * @var string
   */
  public $osid;
  /**
   * @var string
   */
  public $vcenterFolder;
  /**
   * @var string
   */
  public $vcenterUri;
  /**
   * @var string
   */
  public $vcenterVersion;
  /**
   * @var string
   */
  public $vcenterVmId;

  /**
   * @param string
   */
  public function setEsxHyperthreading($esxHyperthreading)
  {
    $this->esxHyperthreading = $esxHyperthreading;
  }
  /**
   * @return string
   */
  public function getEsxHyperthreading()
  {
    return $this->esxHyperthreading;
  }
  /**
   * @param string
   */
  public function setEsxVersion($esxVersion)
  {
    $this->esxVersion = $esxVersion;
  }
  /**
   * @return string
   */
  public function getEsxVersion()
  {
    return $this->esxVersion;
  }
  /**
   * @param string
   */
  public function setOsid($osid)
  {
    $this->osid = $osid;
  }
  /**
   * @return string
   */
  public function getOsid()
  {
    return $this->osid;
  }
  /**
   * @param string
   */
  public function setVcenterFolder($vcenterFolder)
  {
    $this->vcenterFolder = $vcenterFolder;
  }
  /**
   * @return string
   */
  public function getVcenterFolder()
  {
    return $this->vcenterFolder;
  }
  /**
   * @param string
   */
  public function setVcenterUri($vcenterUri)
  {
    $this->vcenterUri = $vcenterUri;
  }
  /**
   * @return string
   */
  public function getVcenterUri()
  {
    return $this->vcenterUri;
  }
  /**
   * @param string
   */
  public function setVcenterVersion($vcenterVersion)
  {
    $this->vcenterVersion = $vcenterVersion;
  }
  /**
   * @return string
   */
  public function getVcenterVersion()
  {
    return $this->vcenterVersion;
  }
  /**
   * @param string
   */
  public function setVcenterVmId($vcenterVmId)
  {
    $this->vcenterVmId = $vcenterVmId;
  }
  /**
   * @return string
   */
  public function getVcenterVmId()
  {
    return $this->vcenterVmId;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(VmwarePlatformDetails::class, 'Google_Service_MigrationCenterAPI_VmwarePlatformDetails');
