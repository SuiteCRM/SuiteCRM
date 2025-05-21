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

namespace Google\Service\GKEOnPrem;

class VmwareVsphereConfig extends \Google\Collection
{
  protected $collection_key = 'tags';
  /**
   * @var string
   */
  public $datastore;
  /**
   * @var string[]
   */
  public $hostGroups;
  protected $tagsType = VmwareVsphereTag::class;
  protected $tagsDataType = 'array';

  /**
   * @param string
   */
  public function setDatastore($datastore)
  {
    $this->datastore = $datastore;
  }
  /**
   * @return string
   */
  public function getDatastore()
  {
    return $this->datastore;
  }
  /**
   * @param string[]
   */
  public function setHostGroups($hostGroups)
  {
    $this->hostGroups = $hostGroups;
  }
  /**
   * @return string[]
   */
  public function getHostGroups()
  {
    return $this->hostGroups;
  }
  /**
   * @param VmwareVsphereTag[]
   */
  public function setTags($tags)
  {
    $this->tags = $tags;
  }
  /**
   * @return VmwareVsphereTag[]
   */
  public function getTags()
  {
    return $this->tags;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(VmwareVsphereConfig::class, 'Google_Service_GKEOnPrem_VmwareVsphereConfig');
