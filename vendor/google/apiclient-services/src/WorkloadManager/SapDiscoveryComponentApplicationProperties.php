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

namespace Google\Service\WorkloadManager;

class SapDiscoveryComponentApplicationProperties extends \Google\Model
{
  /**
   * @var bool
   */
  public $abap;
  /**
   * @var string
   */
  public $appInstanceNumber;
  /**
   * @var string
   */
  public $applicationType;
  /**
   * @var string
   */
  public $ascsInstanceNumber;
  /**
   * @var string
   */
  public $ascsUri;
  /**
   * @var string
   */
  public $ersInstanceNumber;
  /**
   * @var string
   */
  public $kernelVersion;
  /**
   * @var string
   */
  public $nfsUri;

  /**
   * @param bool
   */
  public function setAbap($abap)
  {
    $this->abap = $abap;
  }
  /**
   * @return bool
   */
  public function getAbap()
  {
    return $this->abap;
  }
  /**
   * @param string
   */
  public function setAppInstanceNumber($appInstanceNumber)
  {
    $this->appInstanceNumber = $appInstanceNumber;
  }
  /**
   * @return string
   */
  public function getAppInstanceNumber()
  {
    return $this->appInstanceNumber;
  }
  /**
   * @param string
   */
  public function setApplicationType($applicationType)
  {
    $this->applicationType = $applicationType;
  }
  /**
   * @return string
   */
  public function getApplicationType()
  {
    return $this->applicationType;
  }
  /**
   * @param string
   */
  public function setAscsInstanceNumber($ascsInstanceNumber)
  {
    $this->ascsInstanceNumber = $ascsInstanceNumber;
  }
  /**
   * @return string
   */
  public function getAscsInstanceNumber()
  {
    return $this->ascsInstanceNumber;
  }
  /**
   * @param string
   */
  public function setAscsUri($ascsUri)
  {
    $this->ascsUri = $ascsUri;
  }
  /**
   * @return string
   */
  public function getAscsUri()
  {
    return $this->ascsUri;
  }
  /**
   * @param string
   */
  public function setErsInstanceNumber($ersInstanceNumber)
  {
    $this->ersInstanceNumber = $ersInstanceNumber;
  }
  /**
   * @return string
   */
  public function getErsInstanceNumber()
  {
    return $this->ersInstanceNumber;
  }
  /**
   * @param string
   */
  public function setKernelVersion($kernelVersion)
  {
    $this->kernelVersion = $kernelVersion;
  }
  /**
   * @return string
   */
  public function getKernelVersion()
  {
    return $this->kernelVersion;
  }
  /**
   * @param string
   */
  public function setNfsUri($nfsUri)
  {
    $this->nfsUri = $nfsUri;
  }
  /**
   * @return string
   */
  public function getNfsUri()
  {
    return $this->nfsUri;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(SapDiscoveryComponentApplicationProperties::class, 'Google_Service_WorkloadManager_SapDiscoveryComponentApplicationProperties');
