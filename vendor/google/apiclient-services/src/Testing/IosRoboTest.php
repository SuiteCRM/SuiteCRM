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

namespace Google\Service\Testing;

class IosRoboTest extends \Google\Model
{
  /**
   * @var string
   */
  public $appBundleId;
  protected $appIpaType = FileReference::class;
  protected $appIpaDataType = '';
  protected $roboScriptType = FileReference::class;
  protected $roboScriptDataType = '';

  /**
   * @param string
   */
  public function setAppBundleId($appBundleId)
  {
    $this->appBundleId = $appBundleId;
  }
  /**
   * @return string
   */
  public function getAppBundleId()
  {
    return $this->appBundleId;
  }
  /**
   * @param FileReference
   */
  public function setAppIpa(FileReference $appIpa)
  {
    $this->appIpa = $appIpa;
  }
  /**
   * @return FileReference
   */
  public function getAppIpa()
  {
    return $this->appIpa;
  }
  /**
   * @param FileReference
   */
  public function setRoboScript(FileReference $roboScript)
  {
    $this->roboScript = $roboScript;
  }
  /**
   * @return FileReference
   */
  public function getRoboScript()
  {
    return $this->roboScript;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(IosRoboTest::class, 'Google_Service_Testing_IosRoboTest');
