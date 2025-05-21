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

namespace Google\Service\FirebaseDataConnect;

class GraphqlError extends \Google\Collection
{
  protected $collection_key = 'path';
  protected $extensionsType = GraphqlErrorExtensions::class;
  protected $extensionsDataType = '';
  protected $locationsType = SourceLocation::class;
  protected $locationsDataType = 'array';
  /**
   * @var string
   */
  public $message;
  /**
   * @var array[]
   */
  public $path;

  /**
   * @param GraphqlErrorExtensions
   */
  public function setExtensions(GraphqlErrorExtensions $extensions)
  {
    $this->extensions = $extensions;
  }
  /**
   * @return GraphqlErrorExtensions
   */
  public function getExtensions()
  {
    return $this->extensions;
  }
  /**
   * @param SourceLocation[]
   */
  public function setLocations($locations)
  {
    $this->locations = $locations;
  }
  /**
   * @return SourceLocation[]
   */
  public function getLocations()
  {
    return $this->locations;
  }
  /**
   * @param string
   */
  public function setMessage($message)
  {
    $this->message = $message;
  }
  /**
   * @return string
   */
  public function getMessage()
  {
    return $this->message;
  }
  /**
   * @param array[]
   */
  public function setPath($path)
  {
    $this->path = $path;
  }
  /**
   * @return array[]
   */
  public function getPath()
  {
    return $this->path;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GraphqlError::class, 'Google_Service_FirebaseDataConnect_GraphqlError');
