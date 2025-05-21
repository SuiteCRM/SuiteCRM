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

namespace Google\Service\Gmail;

class KaclsKeyMetadata extends \Google\Model
{
  /**
   * @var string
   */
  public $kaclsData;
  /**
   * @var string
   */
  public $kaclsUri;

  /**
   * @param string
   */
  public function setKaclsData($kaclsData)
  {
    $this->kaclsData = $kaclsData;
  }
  /**
   * @return string
   */
  public function getKaclsData()
  {
    return $this->kaclsData;
  }
  /**
   * @param string
   */
  public function setKaclsUri($kaclsUri)
  {
    $this->kaclsUri = $kaclsUri;
  }
  /**
   * @return string
   */
  public function getKaclsUri()
  {
    return $this->kaclsUri;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(KaclsKeyMetadata::class, 'Google_Service_Gmail_KaclsKeyMetadata');
