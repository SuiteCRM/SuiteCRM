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

namespace Google\Service\BinaryAuthorization;

class CheckSet extends \Google\Collection
{
  protected $collection_key = 'checks';
  protected $checksType = Check::class;
  protected $checksDataType = 'array';
  /**
   * @var string
   */
  public $displayName;
  protected $imageAllowlistType = ImageAllowlist::class;
  protected $imageAllowlistDataType = '';
  protected $scopeType = Scope::class;
  protected $scopeDataType = '';

  /**
   * @param Check[]
   */
  public function setChecks($checks)
  {
    $this->checks = $checks;
  }
  /**
   * @return Check[]
   */
  public function getChecks()
  {
    return $this->checks;
  }
  /**
   * @param string
   */
  public function setDisplayName($displayName)
  {
    $this->displayName = $displayName;
  }
  /**
   * @return string
   */
  public function getDisplayName()
  {
    return $this->displayName;
  }
  /**
   * @param ImageAllowlist
   */
  public function setImageAllowlist(ImageAllowlist $imageAllowlist)
  {
    $this->imageAllowlist = $imageAllowlist;
  }
  /**
   * @return ImageAllowlist
   */
  public function getImageAllowlist()
  {
    return $this->imageAllowlist;
  }
  /**
   * @param Scope
   */
  public function setScope(Scope $scope)
  {
    $this->scope = $scope;
  }
  /**
   * @return Scope
   */
  public function getScope()
  {
    return $this->scope;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(CheckSet::class, 'Google_Service_BinaryAuthorization_CheckSet');
