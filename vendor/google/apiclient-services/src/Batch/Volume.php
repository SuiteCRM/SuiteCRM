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

namespace Google\Service\Batch;

class Volume extends \Google\Collection
{
  protected $collection_key = 'mountOptions';
  /**
   * @var string
   */
  public $deviceName;
  protected $gcsType = GCS::class;
  protected $gcsDataType = '';
  /**
   * @var string[]
   */
  public $mountOptions;
  /**
   * @var string
   */
  public $mountPath;
  protected $nfsType = NFS::class;
  protected $nfsDataType = '';

  /**
   * @param string
   */
  public function setDeviceName($deviceName)
  {
    $this->deviceName = $deviceName;
  }
  /**
   * @return string
   */
  public function getDeviceName()
  {
    return $this->deviceName;
  }
  /**
   * @param GCS
   */
  public function setGcs(GCS $gcs)
  {
    $this->gcs = $gcs;
  }
  /**
   * @return GCS
   */
  public function getGcs()
  {
    return $this->gcs;
  }
  /**
   * @param string[]
   */
  public function setMountOptions($mountOptions)
  {
    $this->mountOptions = $mountOptions;
  }
  /**
   * @return string[]
   */
  public function getMountOptions()
  {
    return $this->mountOptions;
  }
  /**
   * @param string
   */
  public function setMountPath($mountPath)
  {
    $this->mountPath = $mountPath;
  }
  /**
   * @return string
   */
  public function getMountPath()
  {
    return $this->mountPath;
  }
  /**
   * @param NFS
   */
  public function setNfs(NFS $nfs)
  {
    $this->nfs = $nfs;
  }
  /**
   * @return NFS
   */
  public function getNfs()
  {
    return $this->nfs;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Volume::class, 'Google_Service_Batch_Volume');
