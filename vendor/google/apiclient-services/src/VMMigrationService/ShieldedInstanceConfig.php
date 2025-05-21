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

namespace Google\Service\VMMigrationService;

class ShieldedInstanceConfig extends \Google\Model
{
  /**
   * @var bool
   */
  public $enableIntegrityMonitoring;
  /**
   * @var bool
   */
  public $enableVtpm;
  /**
   * @var string
   */
  public $secureBoot;

  /**
   * @param bool
   */
  public function setEnableIntegrityMonitoring($enableIntegrityMonitoring)
  {
    $this->enableIntegrityMonitoring = $enableIntegrityMonitoring;
  }
  /**
   * @return bool
   */
  public function getEnableIntegrityMonitoring()
  {
    return $this->enableIntegrityMonitoring;
  }
  /**
   * @param bool
   */
  public function setEnableVtpm($enableVtpm)
  {
    $this->enableVtpm = $enableVtpm;
  }
  /**
   * @return bool
   */
  public function getEnableVtpm()
  {
    return $this->enableVtpm;
  }
  /**
   * @param string
   */
  public function setSecureBoot($secureBoot)
  {
    $this->secureBoot = $secureBoot;
  }
  /**
   * @return string
   */
  public function getSecureBoot()
  {
    return $this->secureBoot;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ShieldedInstanceConfig::class, 'Google_Service_VMMigrationService_ShieldedInstanceConfig');
